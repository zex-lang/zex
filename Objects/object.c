/*
 * Zex Programming Language
 * Objects/object.c - Base object operations
 */

#include "object.h"
#include "tableobject.h"
#include "stringobject.h"
#include "intobject.h"
#include "floatobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include "funcobject.h"
#include "classobject.h"
#include "memory.h"

/* Global object list for GC */
Obj* vm_objects = NULL;

Obj* allocate_object(size_t size, ObjType type, ObjClass* klass) {
    Obj* object = (Obj*)zex_alloc(size);
    object->type = type;
    object->is_marked = false;
    object->klass = klass;
    
    /* Add to GC tracking list */
    object->next = vm_objects;
    vm_objects = object;
    
    return object;
}

const char* obj_type_name(ObjType type) {
    switch (type) {
        case OBJ_INT:           return "int";
        case OBJ_FLOAT:         return "float";
        case OBJ_BOOL:          return "bool";
        case OBJ_NULL:          return "null";
        case OBJ_STRING:        return "string";
        case OBJ_FUNCTION:      return "function";
        case OBJ_CLASS:         return "class";
        case OBJ_INSTANCE:      return "instance";
        case OBJ_NATIVE:        return "native";
        case OBJ_BOUND_METHOD:  return "bound_method";
        default:                return "unknown";
    }
}

const char* value_type_name(Value value) {
    if (value.obj == NULL) return "null";
    
    if (value.obj->type == OBJ_INSTANCE && value.obj->klass) {
        return value.obj->klass->name->chars;
    }
    
    return obj_type_name(value.obj->type);
}

void print_value(Value value) {
    if (value.obj == NULL) {
        printf("null");
        return;
    }
    
    switch (value.obj->type) {
        case OBJ_INT: {
            ObjInt* obj = (ObjInt*)value.obj;
            printf("%lld", (long long)obj->value);
            break;
        }
        case OBJ_FLOAT: {
            ObjFloat* obj = (ObjFloat*)value.obj;
            printf("%g", obj->value);
            break;
        }
        case OBJ_BOOL: {
            ObjBool* obj = (ObjBool*)value.obj;
            printf("%s", obj->value ? "true" : "false");
            break;
        }
        case OBJ_NULL:
            printf("null");
            break;
        case OBJ_STRING:
            printf("%s", AS_STRING(value)->chars);
            break;
        case OBJ_FUNCTION: {
            ObjFunction* fn = AS_FUNCTION(value);
            if (fn->name == NULL) {
                printf("<script>");
            } else {
                printf("<fun %s>", fn->name->chars);
            }
            break;
        }
        case OBJ_NATIVE:
            printf("<native fn>");
            break;
        case OBJ_CLASS:
            printf("<class %s>", AS_CLASS(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            printf("<%s instance>", AS_INSTANCE(value)->obj.klass->name->chars);
            break;
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = AS_BOUND_METHOD(value);
            if (bound->method->name == NULL) {
                printf("<bound method>");
            } else {
                printf("<bound method %s>", bound->method->name->chars);
            }
            break;
        }
    }
}

ObjString* value_to_string(Value value) {
    char buffer[256];
    int len = 0;
    
    if (value.obj == NULL) {
        return new_string_cstr("null");
    }
    
    switch (value.obj->type) {
        case OBJ_INT: {
            ObjInt* obj = (ObjInt*)value.obj;
            len = snprintf(buffer, sizeof(buffer), "%lld", (long long)obj->value);
            return new_string(buffer, len);
        }
        case OBJ_FLOAT: {
            ObjFloat* obj = (ObjFloat*)value.obj;
            len = snprintf(buffer, sizeof(buffer), "%g", obj->value);
            return new_string(buffer, len);
        }
        case OBJ_BOOL: {
            ObjBool* obj = (ObjBool*)value.obj;
            return new_string_cstr(obj->value ? "true" : "false");
        }
        case OBJ_NULL:
            return new_string_cstr("null");
        case OBJ_STRING:
            return AS_STRING(value);
        case OBJ_FUNCTION: {
            ObjFunction* fn = AS_FUNCTION(value);
            if (fn->name == NULL) {
                return new_string_cstr("<script>");
            }
            len = snprintf(buffer, sizeof(buffer), "<fun %s>", fn->name->chars);
            return new_string(buffer, len);
        }
        case OBJ_NATIVE:
            return new_string_cstr("<native fn>");
        case OBJ_CLASS:
            len = snprintf(buffer, sizeof(buffer), "<class %s>", AS_CLASS(value)->name->chars);
            return new_string(buffer, len);
        case OBJ_INSTANCE:
            len = snprintf(buffer, sizeof(buffer), "<%s instance>", 
                          AS_INSTANCE(value)->obj.klass->name->chars);
            return new_string(buffer, len);
        case OBJ_BOUND_METHOD:
            return new_string_cstr("<bound method>");
    }
    
    return new_string_cstr("<unknown>");
}

bool values_equal(Value a, Value b) {
    if (a.obj == NULL && b.obj == NULL) return true;
    if (a.obj == NULL || b.obj == NULL) return false;
    if (a.obj->type != b.obj->type) return false;
    
    switch (a.obj->type) {
        case OBJ_INT: {
            ObjInt* ia = (ObjInt*)a.obj;
            ObjInt* ib = (ObjInt*)b.obj;
            return ia->value == ib->value;
        }
        case OBJ_FLOAT: {
            ObjFloat* fa = (ObjFloat*)a.obj;
            ObjFloat* fb = (ObjFloat*)b.obj;
            return fa->value == fb->value;
        }
        case OBJ_BOOL: {
            ObjBool* ba = (ObjBool*)a.obj;
            ObjBool* bb = (ObjBool*)b.obj;
            return ba->value == bb->value;
        }
        case OBJ_NULL:   
            return true;
        case OBJ_STRING: 
            return AS_STRING(a) == AS_STRING(b);  /* Interned */
        default:         
            return a.obj == b.obj;  /* Reference equality */
    }
}

bool is_truthy(Value value) {
    if (value.obj == NULL) return false;
    
    switch (value.obj->type) {
        case OBJ_NULL:   return false;
        case OBJ_BOOL: {
            ObjBool* obj = (ObjBool*)value.obj;
            return obj->value;
        }
        case OBJ_INT: {
            ObjInt* obj = (ObjInt*)value.obj;
            return obj->value != 0;
        }
        case OBJ_FLOAT: {
            ObjFloat* obj = (ObjFloat*)value.obj;
            return obj->value != 0.0;
        }
        case OBJ_STRING: return AS_STRING(value)->length > 0;
        default:         return true;
    }
}

static void free_object(Obj* object) {
    switch (object->type) {
        case OBJ_INT:
            zex_free(object, sizeof(ObjInt));
            break;
        case OBJ_FLOAT:
            zex_free(object, sizeof(ObjFloat));
            break;
        case OBJ_BOOL:
            /* Singletons, don't free */
            break;
        case OBJ_NULL:
            /* Singleton, don't free */
            break;
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            zex_free(object, sizeof(ObjString) + string->length + 1);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* fn = (ObjFunction*)object;
            /* chunk is freed separately */
            UNUSED(fn);
            zex_free(object, sizeof(ObjFunction));
            break;
        }
        case OBJ_NATIVE:
            zex_free(object, sizeof(ObjNative));
            break;
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            table_free(&klass->methods);
            table_free(&klass->default_properties);
            zex_free(object, sizeof(ObjClass));
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)object;
            table_free(&instance->properties);
            zex_free(object, sizeof(ObjInstance));
            break;
        }
        case OBJ_BOUND_METHOD:
            zex_free(object, sizeof(ObjBoundMethod));
            break;
    }
}

void free_objects(void) {
    Obj* object = vm_objects;
    while (object != NULL) {
        Obj* next = object->next;
        free_object(object);
        object = next;
    }
    vm_objects = NULL;
}
