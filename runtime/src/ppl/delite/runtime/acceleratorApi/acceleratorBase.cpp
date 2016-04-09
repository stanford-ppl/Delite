#include <string>
using namespace std;

abstract class AcceleratorBase {
  public:
    enum memcpyDirection {
      HOST_TO_DEVICE = 0,
      DEVICE_TO_HOST = 1
    };
    typedef memcpyDirection memcpyDirection_t;

    virtual int init(...);
    virtual void* malloc(size_t size);
    virtual void free(void *ptr);
    virtual void memcpy(void *dst, void *src, size_t size, memcpyDirection_t direction);


}
