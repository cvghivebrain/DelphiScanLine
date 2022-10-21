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
procedure DrawPNG(x1, y1, w, h, x2, y2, sx, sy, t, rep: integer; opa: byte);
procedure DrawWholePNG(x, y, sx, sy, t, rep: integer; opa: byte);
procedure SetPNGReplace(r1, g1, b1, r2, g2, b2: byte);
procedure DrawLine(r, g, b, a: byte; x1, y1, x2, y2: integer);

var
  pic: TImage;
  form: TForm;
  pixelarray: PByteArray;
  scanwidth, actualwidth, actualheight, visiblewidth, visibleheight: integer;
  readpixel: array[0..2] of byte;
  PNG: TPNGImage;
  alpha: PByteArray;
  alphawidth: integer;
  alphachk: boolean;
  incolor, outcolor: TColor;

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
  if (y > visibleheight) or (y < 0) then
    begin
    w := 0;
    y := 0;
    end;
  DrawHLineNow(r,g,b,a,x,y,w); // Draw line with revised position & width.
end;

{ Draw horizontal line without overflow checks. }

procedure DrawHLineNow(r, g, b, a: byte; x, y, w: integer);
var p, i: integer;
begin
  p := (y*scanwidth)+(x*3); // Find address for pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to (w-1) do
      begin
      pixelarray[p+(i*3)] := b; // Write pixel data.
      pixelarray[p+1+(i*3)] := g;
      pixelarray[p+2+(i*3)] := r;
      end
  else
    for i := 0 to (w-1) do
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
  if (x > visiblewidth) or (x < 0) then
    begin
    h := 0;
    x := 0;
    end;
  p := (y*scanwidth)+(x*3); // Find address for pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to (h-1) do
      begin
      pixelarray[p] := b; // Write pixel data.
      pixelarray[p+1] := g;
      pixelarray[p+2] := r;
      p := p+scanwidth; // Jump to next scanline, same y position.
      end
  else
    for i := 0 to (h-1) do
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
  for i := 0 to (h-1) do DrawHLineNow(r,g,b,a,x,y+i,w); // Draw rectangle out of series of lines.
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
  for i := 1 to (visibleheight-1) do // Copy visible lines.
    Move(pixelarray[0],pixelarray[i*scanwidth],(visiblewidth*3));
end;

{ Load PNG. }

procedure LoadSheet(f: string);
begin
  PNG.Free; // Clear previous PNG.
  PNG := TPNGImage.Create; // Initialise PNG.
  PNG.LoadFromFile(f); // Load new PNG.
  if PNG.Header.ColorType = COLOR_RGBALPHA or COLOR_GRAYSCALEALPHA then // Check if PNG has an alpha channel.
    begin
    alpha := PNG.AlphaScanline[0]; // Pointer for alpha channel.
    alphawidth := Longint(PNG.AlphaScanline[1])-Longint(alpha); // Size of alpha for one line.
    alphachk := true;
    end
  else alphachk := false;
end;

{ Draw section of PNG on screen.
    x1, y1: Position on PNG
    w, h: Width/height of section
    x2, y2: Position on screen
    sx, sy: Scale (integers only, can be negative)
    t: Transparency mode (0 = none; 1 = use 0,0 on PNG; 2 = use 0,0 on section; 3 = use alpha channel)
    rep: Replace colour as defined by SetPNGReplace
    opa: Opacity (0 = 0%; 255 = 100%) }

procedure DrawPNG(x1, y1, w, h, x2, y2, sx, sy, t, rep: integer; opa: byte);
var i, r, g, b, a, x, y, xpx, ypx: integer;
  p, p1, p2: TColor;
begin
  p1 := PNG.Pixels[0,0]; // Get pixel on top left of image.
  p2 := PNG.Pixels[x1,y1]; // Get pixel on top left of section.
  for i := 0 to (w*h)-1 do
    begin
    x := x1+(i mod w); // Get position on PNG.
    y := y1+(i div w);
    p := PNG.Pixels[x,y]; // Get pixel as TColor.
    if (rep = 1) and (p = incolor) then p := outcolor; // Replace colour.
    r := GetRValue(p); // Get RGB values.
    g := GetGValue(p);
    b := GetBValue(p);
    case t of
      1: // Use pixel 0,0 as transparent.
        if p = p1 then a := 0 else a := opa;
      2: // Use pixel 0,0 in section as transparent.
        if p = p2 then a := 0 else a := opa;
      3: // Use alpha transparency.
        if alphachk = true then a := alpha[(y*alphawidth)+x]*(opa div 255) // Get alpha value.
        else a := opa;
      else a := opa; // Default no transparency.
    end;
    if sx>0 then xpx := (i mod w)*sx // Set relative position of pixel.
    else xpx := (w-(i mod w))*Abs(sx);
    if sy>0 then ypx := (i div w)*sy
    else ypx := (h-(i div w))*Abs(sy);
    if (Abs(sx)=1) and (Abs(sy)=1) then DrawPixel(r,g,b,a,x2+xpx,y2+ypx)
    else DrawRect(r,g,b,a,x2+xpx,y2+ypx,Abs(sx),Abs(sy));
    end;
end;

{ Draw whole PNG on screen. }

procedure DrawWholePNG(x, y, sx, sy, t, rep: integer; opa: byte);
begin
  DrawPNG(0,0,PNG.Width,PNG.Height,x,y,sx,sy,t,rep,opa);
end;

{ Set colour replacement. Only supports a single colour. }

procedure SetPNGReplace(r1, g1, b1, r2, g2, b2: byte);
begin
  incolor := r1 or (g1 shl 8) or (b1 shl 16); // Convert RGB to TColor.
  outcolor := r2 or (g2 shl 8) or (b2 shl 16);
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

end.