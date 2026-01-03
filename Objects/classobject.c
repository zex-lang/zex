/*
 * Zex Programming Language
 * Objects/classobject.c - Class and instance object implementation
 */

#include "classobject.h"
#include "stringobject.h"
#include "tableobject.h"

ObjClass* new_class(ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS, NULL);
    klass->name = name;
    klass->superclass = NULL;
    klass->init = NULL;
    table_init(&klass->methods);
    table_init(&klass->static_members);
    table_init(&klass->default_properties);
    table_init(&klass->getters);
    table_init(&klass->setters);
    table_init(&klass->member_visibility);
    return klass;
}

ObjInstance* new_instance(ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE, klass);
    table_init(&instance->properties);
    
    /* Copy default property values */
    table_add_all(&klass->default_properties, &instance->properties);
    
    return instance;
}
