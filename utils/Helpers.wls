(* For use with./utils/save_gl_images.gdb *)
Clear[getGlData];
getGlData[glBinPath_] := Module[{dotStr, data, width, height},
  dotStr =
   StringSplit[
    First[StringCases[
      FileBaseName[glBinPath] <> "." <> FileExtension[glBinPath],
      "." ~~ __ ~~ "."]], "."];
  stride = ToExpression@dotStr[[1]];
  width = ToExpression@dotStr[[2]];
  height = ToExpression@dotStr[[3]];
  srcX = ToExpression@dotStr[[4]];
  srcY = ToExpression@dotStr[[5]];
  dstX = ToExpression@dotStr[[6]];
  dstY = ToExpression@dotStr[[7]];
  data = dotStr[[8]];
  <|"data" -> data, "height" -> height, "width" -> width, "stride" -> stride, "srcX" -> srcX, "srcY" -> srcY, "dstX" -> dstX, "dstY" -> dstY|>]

Clear[bgr];
bgr[{r_, g_, b_, a_}] := {b, g, r}

Clear[importImageBinary];
importImageBinary[path_] :=
 Module[{glData, width, height, stride, data, dstX, dstY, srcX, srcY,
   img},
  glData = getGlData[path];
  width = glData[["width"]];
  height = glData[["height"]];
  data = glData[["data"]];
  stride = glData[["stride"]];
  srcX = glData[["srcX"]];
  srcY = glData[["srcY"]];
  dstX = glData[["dstX"]];
  dstY = glData[["dstY"]];
  img = Image[
    ArrayReshape[Import[path, "Binary"], {height, stride/4, 4}],
    "Byte", ColorSpace -> "RGB"];
  Tooltip[ColorCombine[bgr[ColorSeparate[img]]], path]]