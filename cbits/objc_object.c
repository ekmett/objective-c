#include "objc_object.h"

static SEL selFree = 0;
static IMP impFree = 0;
void freeObject(id a) {
  if (!impFree) {
    selFree = sel_registerName("free:");
    impFree = class_getMethodImplementation((Class)objc_getClass("Object"), selFree);
  }
  impFree(a, selFree);
}
