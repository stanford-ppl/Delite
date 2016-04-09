#include<stdio.h>

#define PREFIX_foo(pfix) \
    foo_##pfix

//void maxjMemcpy(void *dst, void *src, size_t size, int direction) {
//  PREFIX(Top)_writeLMem_actions_t wrAct;
//  wrAct.param_size = size;
//  wrAct.param_start = dst;
//  wrAct.instream_fromcpu = src;
//  Top_writeLMem_run(engine, &wrAct);
//}

//void maxjInit() {
//}

void (*maxjInit)();
void (*maxjDestroy)();
void (*maxjAlloc)();
void (*maxjFree)();
void (*maxjMemcpy)();
void (*maxjLaunch)();
void (*maxjGetDeviceInfo)();

void foo_s1() {
  printf("foo_s1");
}

void foo_s2() {
  printf("foo_s2");
}

int main() {
  void (*s1_func)() = PREFIX_foo(s1);
  void (*s2_func)() = PREFIX_foo(s2);
  s1_func();
  s2_func();
  return 0;
}
