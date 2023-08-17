#include "Raw.chs.h"
extern __attribute__((__visibility__("default"))) char __c2hs_wrapped__igSelectable_Bool(const char * label,
                                                                                         char selected,
                                                                                         ImGuiSelectableFlags flags,
                                                                                         const ImVec2 * size)
{
    return igSelectable_Bool(label, selected, flags, *size);
}
extern __attribute__((__visibility__("default"))) char __c2hs_wrapped__igButton(const char * label,
                                                                                const ImVec2 * size)
{
    return igButton(label, *size);
}
extern __attribute__((__visibility__("default"))) char __c2hs_wrapped__igBeginListBox(const char * label,
                                                                                      const ImVec2 * size)
{
    return igBeginListBox(label, *size);
}
extern __attribute__((__visibility__("default"))) char __c2hs_wrapped__igBegin(const char * name,
                                                                               _Bool * p_open,
                                                                               ImGuiWindowFlags flags)
{
    return igBegin(name, p_open, flags);
}
