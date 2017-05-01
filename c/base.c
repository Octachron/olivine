#include <vulkan/vulkan.h>
int main(int nargs, char** args) {


const char *extensionNames[] = { "VK_KHR_surface", "VK_KHR_win32_surface" };

VkInstanceCreateInfo instanceCreateInfo = {
  VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO, // VkStructureType sType;
  NULL, // const void* pNext;
  0,    // VkInstanceCreateFlags flags;
  NULL, // const VkApplicationInfo* pApplicationInfo;
  0,    // uint32_t enabledLayerNameCount;
  NULL, // const char* const* ppEnabledLayerNames;
  2,    // uint32_t enabledExtensionNameCount;
  extensionNames, // const char* const* ppEnabledExtensionNames;
};

 VkInstance c;
 VkResult r = VkCreateInstance(&instanceCreateInfo, NULL, &c);

}
