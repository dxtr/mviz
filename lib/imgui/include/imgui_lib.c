#ifndef IMGUI_LIB
#define IMGUI_LIB

#include <stdbool.h>
#include <cimgui.h>

inline bool igButton_wrapper(const char *label, const ImVec2 *size) {
  return igButton(label, *size);
}

inline bool igBeginListBox_wrapper(const char *label, const ImVec2 *size) {
  return igBeginListBox(label, *size);
}

inline bool igSelectable_wrapper(const char* label, bool selected, ImGuiSelectableFlags flags, const ImVec2 *size) {
  return igSelectable_Bool(label, selected, flags, *size);
}

#endif
