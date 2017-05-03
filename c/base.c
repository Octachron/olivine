#include <vulkan/vulkan.h>
#include <stdio.h>
#include <stdlib.h>

void test(VkResult r) {
  if (r < 0) {
      printf("Failed with error code %d\n",r);
      exit(1);
    }
}

int main(int nargs, char** args) {

  uint32_t nExtensions= 0;
  const char *extensionNames[0] = {};

  VkInstanceCreateInfo instanceCreateInfo = {
    VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO, // VkStructureType sType;
    NULL, // const void* pNext;
    0,    // VkInstanceCreateFlags flags;
    NULL, // const VkApplicationInfo* pApplicationInfo;
    0,    // uint32_t enabledLayerNameCount;
    NULL, // const char* const* ppEnabledLayerNames;
    nExtensions,    // uint32_t enabledExtensionNameCount;
    extensionNames, // const char* const* ppEnabledExtensionNames;
  };

 VkInstance c;
 VkResult r = vkCreateInstance(&instanceCreateInfo, NULL, &c);
 test(r);

 int count = 5;
 VkPhysicalDevice devices[count];

 r = vkEnumeratePhysicalDevices(c,&count,devices);
 test(r);
 printf ("Found %d devices\n", count);

}
