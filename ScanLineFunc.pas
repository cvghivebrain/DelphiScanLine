unit ScanLineFunc;

interface

uses Forms, Graphics, ExtCtrls, SysUtils, pngimage, Windows, Math;

procedure InitImage(frm: TForm; img: TImage);
procedure MatchWindow;
procedure DrawPixel(r, g, b, a: byte; x, y: integer);
procedure GetPixel(x, y: integer);
function AlphaBlend(c1, a1, c2: integer): byte;
procedure DrawHLine(r, g, b, a: byte; x, y, w: integer);
procedure DrawHLineNow(r, g, b, a: byte; x, y, w: integer);
procedure DrawVLine(r, g, b, a: byte; x, y, h: integer);
procedure DrawRect(r, g, b, a: byte; x, y, w, h: integer);
procedure DrawBox(r, g, b, a: byte; x, y, w, h: integer);
procedure DrawBoxFill(r, g, b, a, r2, g2, b2, a2: byte; x, y, w, h: integer);
procedure DrawBox2(r, g, b, a: byte; x, y, w, h, t: integer);
procedure DrawBoxFill2(r, g, b, a, r2, g2, b2, a2: byte; x, y, w, h, t: integer);
procedure FillScreen(r, g, b: byte);
procedure LoadSheet(f: string);
procedure DrawPNG(x1, y1, w, h, x2, y2, sx, sy, t: integer; opa, r_tint, g_tint, b_tint: byte);
procedure DrawScaledPNG(x1, y1, w1, h1, x2, y2, w2, h2, t: integer; opa, r_tint, g_tint, b_tint: byte);
procedure DrawWholePNG(x, y, sx, sy, t: integer; opa, r_tint, g_tint, b_tint: byte);
procedure DrawLine(r, g, b, a: byte; x1, y1, x2, y2: integer);
procedure DrawTriangleFlat(r, g, b, a: byte; xtop, ytop, xleft, xright, ybtm, trim: integer);
procedure DrawTriangle(r, g, b, a: byte; x1, y1, x2, y2, x3, y3: integer);
procedure DrawGrid(r, g, b, a: byte; x, y, w, h, cx, cy: integer; outer: boolean);
procedure DrawRectStriped(r1, g1, b1, a1, r2, g2, b2, a2: byte; x, y, w, h, t1, t2: integer; vert: boolean);

var
  pic: TImage;
  form: TForm;
  pixelarray: PByteArray;
  scanwidth, actualwidth, actualheight, visiblewidth, visibleheight: integer;
  readpixel: array[0..2] of byte;
  PNG: TPNGImage;
  alpha: PByteArray;
  alphawidth: integer;
  alphachk, singletrans: boolean;

implementation

{ Initialise image. }

procedure InitImage(frm: TForm; img: TImage);
begin
  form := frm; // Assign form.
  pic := img; // Assign image.
  pic.Picture.Bitmap.PixelFormat := pf24bit; // Set main bitmap to 32-bit RGBA.
  actualwidth := Screen.Width; // Set max dimensions to match the whole screen.
  actualheight := Screen.Height;
  pic.Picture.Bitmap.Width := actualwidth;
  pic.Picture.Bitmap.Height := actualheight;
  MatchWindow;
  pixelarray := pic.Picture.Bitmap.ScanLine[0]; // Get pointer for pixels.
  scanwidth := Longint(pic.Picture.Bitmap.ScanLine[1])-Longint(pixelarray); // Get scanline width (+ padding).
end;

{ Match image boundaries with visible window. }

procedure MatchWindow;
begin
  visiblewidth := form.ClientWidth; // Get window size.
  visibleheight := form.ClientHeight;
  pic.Width := visiblewidth; // Set image boundaries.
  pic.Height := visibleheight;
end;

{ Draw single pixel. }

procedure DrawPixel(r, g, b, a: byte; x, y: integer);
var p: integer;
begin
  p := (y*scanwidth)+(x*3); // Find address for pixel.
  if (x>-1) and (y>-1) and (x<visiblewidth) and (y<visibleheight) then
    if a = 255 then // Check alpha value (255 is opaque).
      begin
      pixelarray[p] := b; // Write pixel data.
      pixelarray[p+1] := g;
      pixelarray[p+2] := r;
      end
    else
      begin
      pixelarray[p] := AlphaBlend(b, a, pixelarray[p]); // Write pixel data.
      pixelarray[p+1] := AlphaBlend(g, a, pixelarray[p+1]);
      pixelarray[p+2] := AlphaBlend(r, a, pixelarray[p+2]);
      end;
end;

{ Copy RBGA values of specified pixel to array. }

procedure GetPixel(x, y: integer);
var p: integer;
begin
  p := (y*scanwidth)+(x*3); // Find address for pixel.
  readpixel[0] := pixelarray[p]; // Copy blue value.
  readpixel[1] := pixelarray[p+1]; // Copy green value.
  readpixel[2] := pixelarray[p+2]; // Copy red value.
end;

{ Blend two colour values with alpha. }

function AlphaBlend(c1, a1, c2: integer): byte;
begin
  result := (((c1+1)*a1)+(c2*(256-a1))) shr 8;
end;

{ Draw horizontal line. }

procedure DrawHLine(r, g, b, a: byte; x, y, w: integer);
begin
  if w < 0 then // Check if width is negative.
    begin
    x := x+w; // Flip.
    w := abs(w); // Make positive.
    end;
  if x+w > visiblewidth then w := visiblewidth-x; // Trim line if it overflows.
  if x < 0 then // Check if position is negative.
    begin
    w := w+x; // Trim line.
    x := 0; // Align to edge.
    end;
  if (y > visibleheight) or (y < 0) or (w < 1) then exit; // Do nothing if not visible.
  DrawHLineNow(r,g,b,a,x,y,w); // Draw line with revised position & width.
end;

{ Draw horizontal line without overflow checks. }

procedure DrawHLineNow(r, g, b, a: byte; x, y, w: integer);
var p, i: integer;
begin
  p := (y*scanwidth)+(x*3); // Find address for pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to w-1 do
      begin
      pixelarray[p+(i*3)] := b; // Write pixel data.
      pixelarray[p+1+(i*3)] := g;
      pixelarray[p+2+(i*3)] := r;
      end
  else
    for i := 0 to w-1 do
      begin
      pixelarray[p+(i*3)] := AlphaBlend(b, a, pixelarray[p+(i*3)]); // Write pixel data.
      pixelarray[p+1+(i*3)] := AlphaBlend(g, a, pixelarray[p+1+(i*3)]);
      pixelarray[p+2+(i*3)] := AlphaBlend(r, a, pixelarray[p+2+(i*3)]);
      end;
end;

{ Draw vertical line. }

procedure DrawVLine(r, g, b, a: byte; x, y, h: integer);
var p, i: integer;
begin
  if h < 0 then // Check if height is negative.
    begin
    y := y+h; // Flip.
    h := abs(h); // Make positive.
    end;
  if y+h > visibleheight then h := visibleheight-y; // Trim line if it overflows.
  if y < 0 then // Check if position is negative.
    begin
    h := h+y; // Trim line.
    y := 0; // Align to edge.
    end;
  if (x > visiblewidth) or (x < 0) or (h < 1) then exit; // Do nothing if not visible.
  p := (y*scanwidth)+(x*3); // Find address for pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to h-1 do
      begin
      pixelarray[p] := b; // Write pixel data.
      pixelarray[p+1] := g;
      pixelarray[p+2] := r;
      p := p+scanwidth; // Jump to next scanline, same y position.
      end
  else
    for i := 0 to h-1 do
      begin
      pixelarray[p] := AlphaBlend(b, a, pixelarray[p]); // Write pixel data.
      pixelarray[p+1] := AlphaBlend(g, a, pixelarray[p+1]);
      pixelarray[p+2] := AlphaBlend(r, a, pixelarray[p+2]);
      p := p+scanwidth; // Jump to next scanline, same y position.
      end;
end;

{ Draw solid rectangle. }

procedure DrawRect(r, g, b, a: byte; x, y, w, h: integer);
var i: integer;
begin
  if x < 0 then // Check if position is negative.
    begin
    w := w+x; // Trim rectangle.
    x := 0; // Align to edge.
    end;
  if y < 0 then
    begin
    h := h+y;
    y := 0;
    end;
  if x+w > visiblewidth then w := visiblewidth-x; // Trim rectangle if it overflows.
  if y+h > visibleheight then h := visibleheight-y;
  for i := 0 to h-1 do DrawHLineNow(r,g,b,a,x,y+i,w); // Draw rectangle out of series of lines.
end;

{ Draw empty box. }

procedure DrawBox(r, g, b, a: byte; x, y, w, h: integer);
begin
  DrawHLine(r, g, b, a, x, y, w); // Top.
  DrawVLine(r, g, b, a, x, y+1, h-2); // Left.
  DrawVLine(r, g, b, a, x+w-1, y+1, h-2); // Right.
  DrawHline(r, g, b, a, x, y+h-1, w); // Bottom.
end;

{ Draw filled box. }

procedure DrawBoxFill(r, g, b, a, r2, g2, b2, a2: byte; x, y, w, h: integer);
begin
  DrawBox(r, g, b, a, x, y, w, h); // Draw box.
  DrawRect(r2, g2, b2, a2, x+1, y+1, w-2, h-2); // Fill it.
end;

{ Draw empty box with thicker lines. }

procedure DrawBox2(r, g, b, a: byte; x, y, w, h, t: integer);
var i: integer;
begin
  for i:= 0 to (t-1) do
    DrawBox(r,g,b,a,x+i,y+i,w-(i shl 1),h-(i shl 1)); // Draw concentric boxes.
end;

{ Draw filled box with thicker lines. }

procedure DrawBoxFill2(r, g, b, a, r2, g2, b2, a2: byte; x, y, w, h, t: integer);
begin
  DrawBox2(r, g, b, a, x, y, w, h, t); // Draw box.
  DrawRect(r2, g2, b2, a2, x+t, y+t, w-(t shl 1), h-(t shl 1)); // Fill it.
end;

{ Fill screen with one colour. }

procedure FillScreen(r, g, b: byte);
var i: integer;
begin
  DrawHLineNow(r, g, b, 255, 0, 0, visiblewidth); // Fill first visible line.
  for i := 1 to visibleheight-1 do // Copy visible lines.
    Move(pixelarray[0],pixelarray[i*scanwidth],(visiblewidth*3));
end;

{ Load PNG. }

procedure LoadSheet(f: string);
begin
  PNG.Free; // Clear previous PNG.
  PNG := TPNGImage.Create; // Initialise PNG.
  PNG.LoadFromFile(f); // Load new PNG.
  if PNG.Header.ColorType in [COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA] then // Check if PNG has an alpha channel.
    begin
    alpha := PNG.AlphaScanline[0]; // Pointer for alpha channel.
    alphawidth := Longint(PNG.AlphaScanline[1])-Longint(alpha); // Size of alpha for one line.
    alphachk := true;
    end
  else alphachk := false;
  if (PNG.Header.ColorType = COLOR_PALETTE) and (PNG.TransparencyMode = ptmBit) then // Check if PNG has indexed transparency.
    singletrans := true
  else singletrans := false;
end;

{ Draw section of PNG on screen.
    x1, y1: Position on PNG
    w, h: Width/height of section
    x2, y2: Position on screen
    sx, sy: Scale (integers only, can be negative)
    t: Transparency mode (0 = none; 1 = use 0,0 on PNG; 2 = use 0,0 on section; 3 = use PNG transparency)
    r_tint, g_tint, b_tint: Darken RGB values (0 = black; 128 = half brightness; 255 = normal)
    opa: Opacity (0 = 0%; 255 = 100%) }

procedure DrawPNG(x1, y1, w, h, x2, y2, sx, sy, t: integer; opa, r_tint, g_tint, b_tint: byte);
var i, j, r, g, b, a, x, y, xpx, ypx: integer;
  p, p1, p2: TColor;
begin
  p1 := PNG.Pixels[0,0]; // Get pixel on top left of image.
  p2 := PNG.Pixels[x1,y1]; // Get pixel on top left of section.
  for i := 0 to h-1 do
    for j := 0 to w-1 do
      begin
      x := x1+j; // Get position on PNG.
      y := y1+i;
      p := PNG.Pixels[x,y]; // Get pixel as TColor.
      r := Trunc(GetRValue(p)*(r_tint/255)); // Get RGB values.
      g := Trunc(GetGValue(p)*(g_tint/255));
      b := Trunc(GetBValue(p)*(b_tint/255));
      case t of
        1: // Use pixel 0,0 as transparent.
          if p = p1 then a := 0 else a := opa;
        2: // Use pixel 0,0 in section as transparent.
          if p = p2 then a := 0 else a := opa;
        3: // Use alpha transparency.
          if alphachk then a := Trunc(alpha[(y*alphawidth)+x]*(opa/255)) // Get alpha value.
          else if (p = PNG.TransparentColor) and singletrans then a := 0 // Check for single-colour transparency.
          else a := opa;
        else a := opa; // Default no transparency.
      end;
      if sx > 0 then xpx := j*sx // Set relative position of pixel.
      else xpx := (w-j)*Abs(sx);
      if sy > 0 then ypx := i*sy
      else ypx := (h-i)*Abs(sy);
      if (Abs(sx) = 1) and (Abs(sy) = 1) then DrawPixel(r,g,b,a,x2+xpx,y2+ypx)
      else DrawRect(r,g,b,a,x2+xpx,y2+ypx,Abs(sx),Abs(sy));
      end;
end;

{ Draw section of PNG on screen at any scale.
    x1, y1: Position on PNG
    w1, h1: Width/height of section
    x2, y2: Position on screen
    w2, h2: Width/height to draw (can be negative)
    t: Transparency mode (0 = none; 1 = use 0,0 on PNG; 2 = use 0,0 on section; 3 = use PNG transparency)
    r_tint, g_tint, b_tint: Darken RGB values (0 = black; 128 = half brightness; 255 = normal)
    opa: Opacity (0 = 0%; 255 = 100%) }

procedure DrawScaledPNG(x1, y1, w1, h1, x2, y2, w2, h2, t: integer; opa, r_tint, g_tint, b_tint: byte);
var i, j, r, g, b, a, x, y, xpx, ypx: integer;
  p, p1, p2: TColor;
  xflip, yflip: boolean;
begin
  p1 := PNG.Pixels[0,0]; // Get pixel on top left of image.
  p2 := PNG.Pixels[x1,y1]; // Get pixel on top left of section.
  if w2 < 0 then
    begin
    w2 := -w2;
    xflip := true; // xflip if width is negative.
    end
  else xflip := false;
  if h2 < 0 then
    begin
    h2 := -h2;
    yflip := true; // yflip if height is negative.
    end
  else yflip := false;
  for i := 0 to h2-1 do
    for j := 0 to w2-1 do
      begin
      x := x1+Trunc((j/w2)*w1); // Get position on PNG.
      y := y1+Trunc((i/h2)*h1);
      p := PNG.Pixels[x,y]; // Get pixel as TColor.
      r := Trunc(GetRValue(p)*(r_tint/255)); // Get RGB values.
      g := Trunc(GetGValue(p)*(g_tint/255));
      b := Trunc(GetBValue(p)*(b_tint/255));
      case t of
        1: // Use pixel 0,0 as transparent.
          if p = p1 then a := 0 else a := opa;
        2: // Use pixel 0,0 in section as transparent.
          if p = p2 then a := 0 else a := opa;
        3: // Use alpha transparency.
          if alphachk then a := Trunc(alpha[(y*alphawidth)+x]*(opa/255)) // Get alpha value.
          else if (p = PNG.TransparentColor) and singletrans then a := 0 // Check for single-colour transparency.
          else a := opa;
        else a := opa; // Default no transparency.
      end;
      if not xflip then xpx := j else xpx := w2-j;
      if not yflip then ypx := i else ypx := h2-i;
      DrawPixel(r,g,b,a,x2+xpx,y2+ypx);
      end;
end;

{ Draw whole PNG on screen. }

procedure DrawWholePNG(x, y, sx, sy, t: integer; opa, r_tint, g_tint, b_tint: byte);
begin
  DrawPNG(0,0,PNG.Width,PNG.Height,x,y,sx,sy,t,opa,r_tint,g_tint,b_tint);
end;

{ Draw a point-to-point line. }

procedure DrawLine(r, g, b, a: byte; x1, y1, x2, y2: integer);
var dx, dy, i: integer;
begin
  dx := x2-x1;
  dy := y2-y1;
  if Abs(dx) < Abs(dy) then // Draw line vertically.
    for i := 0 to Abs(dy)-1 do
      DrawPixel(r,g,b,a,x1+((dx*i) div Abs(dy)),y1+(i*Sign(dy)))
  else // Draw line horizontally.
    for i := 0 to Abs(dx)-1 do
      DrawPixel(r,g,b,a,x1+(i*Sign(dx)),y1+((dy*i) div Abs(dx)));
end;

{ Draw triangle with flat base. }

procedure DrawTriangleFlat(r, g, b, a: byte; xtop, ytop, xleft, xright, ybtm, trim: integer);
var dx, dy, w, i: integer;
begin
  if xleft > xright then // Check if xleft is on the left.
    begin
    i := xleft;
    xleft := xright;
    xright := i; // Swap xleft and xright if they're the wrong way round.
    end;
  dx := xleft-xtop;
  dy := ybtm-ytop;
  w := xright-xleft;
  for i := 0 to Abs(dy)-trim do
    DrawHLine(r,g,b,a,xtop+((dx*i) div Abs(dy)),ytop+(i*Sign(dy)),(w*i) div Abs(dy));
end;

{ Draw any triangle. }

procedure DrawTriangle(r, g, b, a: byte; x1, y1, x2, y2, x3, y3: integer);
var xtop, ytop, xmid, ymid, xbtm, ybtm, xmid2: integer;
begin
  if y1 = y2 then DrawTriangleFlat(r,g,b,a,x3,y3,x1,x2,y1,0) // Check if triangle has a flat base.
  else if y1 = y3 then DrawTriangleFlat(r,g,b,a,x2,y2,x1,x3,y1,0)
  else if y2 = y3 then DrawTriangleFlat(r,g,b,a,x1,y1,x2,x3,y2,0)
  else
    begin
    if (y1 < y2) and (y1 < y3) then // Find highest vertex.
      begin
      ytop := y1;
      xtop := x1;
      end
    else if (y2 < y1) and (y2 < y3) then
      begin
      ytop := y2;
      xtop := x2;
      end
    else
      begin
      ytop := y3;
      xtop := x3;
      end;
    if (y1 > y2) and (y1 < y3) then // Find middle vertex.
      begin
      ymid := y1;
      xmid := x1;
      end
    else if (y2 > y1) and (y2 < y3) then
      begin
      ymid := y2;
      xmid := x2;
      end
    else
      begin
      ymid := y3;
      xmid := x3;
      end;
    if (y1 > y2) and (y1 > y3) then // Find lowest vertex.
      begin
      ybtm := y1;
      xbtm := x1;
      end
    else if (y2 > y1) and (y2 > y3) then
      begin
      ybtm := y2;
      xbtm := x2;
      end
    else
      begin
      ybtm := y3;
      xbtm := x3;
      end;
    xmid2 := Trunc(xtop+((ymid-ytop)/(ybtm-ytop))*(xbtm-xtop)); // Get middle intersection point.
    DrawTriangleFlat(r,g,b,a,xtop,ytop,xmid,xmid2,ymid,0); // Draw top triangle.
    DrawTriangleFlat(r,g,b,a,xbtm,ybtm,xmid,xmid2,ymid,1); // Draw bottom triangle.
    end;
end;

{ Draw a grid. }

procedure DrawGrid(r, g, b, a: byte; x, y, w, h, cx, cy: integer; outer: boolean);
var i, j, cw, ch: integer;
begin
  if outer then DrawBox(r,g,b,a,x,y,w+1,h+1); // Draw outer box.
  cw := w div cx; // Cell width.
  ch := h div cy; // Cell height.
  for i := 1 to cy-1 do
    DrawHLine(r,g,b,a,x+1,y+(i*ch),w); // Draw horizontal lines.
  for i := 0 to cy-1 do
    for j := 1 to cx-1 do
      DrawVLine(r,g,b,a,x+(j*cw),y+(i*ch)+1,ch-1); // Draw vertical lines.
end;

{ Draw a rectangle with horizontal stripes. }

procedure DrawRectStriped(r1, g1, b1, a1, r2, g2, b2, a2: byte; x, y, w, h, t1, t2: integer; vert: boolean);
var i: integer;
begin
  i := 0;
  if vert then // Vertical or horizontal stripes.
    begin
    while i < w do
      begin
      DrawRect(r1,g1,b1,a1,x+i,y,Min(t1,w-i),h); // Draw even stripe.
      i := i+t1;
      DrawRect(r2,g2,b2,a2,x+i,y,Min(t2,w-i),h); // Draw odd stripe.
      i := i+t2;
      end;
    end
  else
    while i < h do
      begin
      DrawRect(r1,g1,b1,a1,x,y+i,w,Min(t1,h-i)); // Draw even stripe.
      i := i+t1;
      DrawRect(r2,g2,b2,a2,x,y+i,w,Min(t2,h-i)); // Draw odd stripe.
      i := i+t2;
      end;
end;

end.