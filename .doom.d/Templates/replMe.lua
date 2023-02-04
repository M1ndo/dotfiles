function replace_keywords(s)
  s = string.gsub(s, "TODO", "\\myk{\\textcolor{DarkOrchid1}{TODO}}")
  s = string.gsub(s, "DONE", "\\myk{\\textcolor{SeaGreen1}{DONE}}")
  s = string.gsub(s, "PROJ", "\\myk{\\textcolor{Aquamarine2}{PROJ}}")
  s = string.gsub(s, "STRT", "\\myk{\\textcolor{OliveDrab1}{STRT}}")
  s = string.gsub(s, "WAIT", "\\myk{\\textcolor{Gold1}{WAIT}}")
  s = string.gsub(s, "HOLD", "\\myk{\\textcolor{MediumPurple1}{HOLD}}")
  s = string.gsub(s, "IDEA", "\\myk{\\textcolor{Orchid2}{IDEA}}")
  s = string.gsub(s, "GYM", "\\myk{\\textcolor{Brown1}{GYM}}")
  s = string.gsub(s, "CANCELLED", "\\myk{\\textcolor{Firebrick2}{CANCELLED}}")
  s = string.gsub(s, "KILL", "\\myk{\\textcolor{Firebrick4}{KILL}}")
  s = string.gsub(s, "OKAY", "\\myk{\\textcolor{LimeGreen}{OKAY}}")
  s = string.gsub(s, "NO", "\\myk{\\textcolor{HotPink1}{NO}}")
  s = string.gsub(s, "YES", "\\myk{\\textcolor{PaleGreen1}{YES}}")
  return s
end
return { replaceMe = replace_keywords }
