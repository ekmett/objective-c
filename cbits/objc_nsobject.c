#include "objc_nsobject.h"

static SEL selRelease = 0;
static IMP impRelease = 0;
void releaseNSObject(id a) {
  if (!impRelease) {
    selRelease = sel_registerName("release:");
    impRelease = class_getMethodImplementation((Class)objc_getClass("NSObject"), selRelease);
  }
  impRelease(a, selRelease);
}

static SEL selRetain = 0;
static IMP impRetain = 0;
void retainNSObject(id a) {
  if (!impRetain) {
    selRetain  = sel_registerName("retain:");
    impRetain = class_getMethodImplementation((Class)objc_getClass("NSObject"), selRetain);
  }
  impRetain(a, selRetain);
}
