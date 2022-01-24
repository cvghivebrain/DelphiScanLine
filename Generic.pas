unit Generic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    imgMain: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    procedure DrawPixel(r, g, b, a: byte; x, y: integer);
    procedure GetPixel(x, y: integer);
    function AlphaBlend(c1, a1, c2: integer): byte;
    procedure DrawHLine(r, g, b, a: byte; x, y, w: integer);  
    procedure DrawVLine(r, g, b, a: byte; x, y, h: integer);  
    procedure DrawRect(r, g, b, a: byte; x, y, w, h: integer);
    procedure DrawBox(r, g, b, a: byte; x, y, w, h: integer);   
    procedure DrawBoxFill(r, g, b, a, r2, g2, b2, a2: byte; x, y, w, h: integer);  
    procedure DrawBox2(r, g, b, a: byte; x, y, w, h, t: integer);
    procedure DrawBoxFill2(r, g, b, a, r2, g2, b2, a2: byte; x, y, w, h, t: integer);
    procedure FillScreen(r, g, b: byte);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pixelarray: PByteArray;
  scanwidth, actualwidth, actualheight, visiblewidth, visibleheight: integer;
  readpixel: array[0..3] of byte;

implementation

{$R *.dfm}

{ Form management. }

procedure TForm1.FormCreate(Sender: TObject);
begin
  imgMain.Picture.Bitmap.PixelFormat := pf32bit; // Set main bitmap to 32-bit RGBA.
  actualwidth := Screen.Width; // Set dimensions to match the screen.
  actualheight := Screen.Height;
  visiblewidth := Form1.ClientWidth; // Set boundaries to match window.
  visibleheight := Form1.ClientHeight;
  imgMain.Picture.Bitmap.Width := actualwidth;
  imgMain.Picture.Bitmap.Height := actualheight;
  imgMain.Width := visiblewidth;
  imgMain.Height := visibleheight;
  pixelarray := imgMain.Picture.Bitmap.ScanLine[0]; // Get pointer for pixels.
  scanwidth := Longint(imgMain.Picture.Bitmap.ScanLine[1])-Longint(pixelarray); // Get scanline width (+ padding).
  FillScreen(0,0,0);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  visiblewidth := Form1.ClientWidth; // Set boundaries to match window.
  visibleheight := Form1.ClientHeight;
  imgMain.Width := visiblewidth;
  imgMain.Height := visibleheight;
  FillScreen(0,0,0);
  DrawRect(0,255,0,255,0,0,50,50);
  DrawRect(0,255,0,255,visiblewidth-50,visibleheight-50,50,50);
end;

{ Pixel operations. }

procedure TForm1.DrawPixel(r, g, b, a: byte; x, y: integer); // Draw single pixel.
var p: integer;
begin
  p := (y*scanwidth)+(x shl 2); // Find address for pixel.    
  if a = 255 then // Check alpha value (255 is opaque).
    begin
    pixelarray[p] := b; // Write pixel data.
    pixelarray[p+1] := g;
    pixelarray[p+2] := r;
    pixelarray[p+3] := 255;
    end
  else
    begin
    pixelarray[p] := AlphaBlend(b, a, pixelarray[p]); // Write pixel data.
    pixelarray[p+1] := AlphaBlend(g, a, pixelarray[p+1]);
    pixelarray[p+2] := AlphaBlend(r, a, pixelarray[p+2]);
    pixelarray[p+3] := 255;
    end;
end;

procedure TForm1.GetPixel(x, y: integer); // Copy RBGA values of specified pixel to array.
var p: integer;
begin
  p := (y*scanwidth)+(x shl 2); // Find address for pixel.
  readpixel[0] := pixelarray[p]; // Copy blue value.
  readpixel[1] := pixelarray[p+1]; // Copy green value.
  readpixel[2] := pixelarray[p+2]; // Copy red value.
  readpixel[3] := pixelarray[p+3]; // Copy alpha value.
end;

function TForm1.AlphaBlend(c1, a1, c2: integer): byte; // Blend two colour values with alpha.
begin
  result := (((c1+1)*a1)+(c2*(256-a1))) shr 8;
end;

procedure TForm1.DrawHLine(r, g, b, a: byte; x, y, w: integer); // Draw horizontal line.
var p, i: integer;
begin
  if w < 0 then // Check if width is negative.
    begin
    x := x+w; // Flip.
    w := abs(w); // Make positive.
    end;
  if x+w > actualwidth then w := actualwidth-x; // Trim line if it overflows.
  if x < 0 then // Check if position is negative.
    begin
    w := w+x; // Trim line.
    x := 0; // Align to edge.
    end;
  if (y > actualheight) or (y < 0) then
    begin
    w := 0;
    y := 0;
    end;
  p := (y*scanwidth)+(x shl 2); // Find address for pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to (w-1) do
      begin
      pixelarray[p+(i shl 2)] := b; // Write pixel data.
      pixelarray[p+1+(i shl 2)] := g;
      pixelarray[p+2+(i shl 2)] := r;
      pixelarray[p+3+(i shl 2)] := 255;
      end
  else
    for i := 0 to (w-1) do
      begin
      pixelarray[p+(i shl 2)] := AlphaBlend(b, a, pixelarray[p+(i shl 2)]); // Write pixel data.
      pixelarray[p+1+(i shl 2)] := AlphaBlend(g, a, pixelarray[p+1+(i shl 2)]);
      pixelarray[p+2+(i shl 2)] := AlphaBlend(r, a, pixelarray[p+2+(i shl 2)]);
      pixelarray[p+3+(i shl 2)] := 255;
      end;
end;

procedure TForm1.DrawVLine(r, g, b, a: byte; x, y, h: integer); // Draw vertical line.
var p, i: integer;
begin   
  if h < 0 then // Check if height is negative.
    begin
    y := y+h; // Flip.
    h := abs(h); // Make positive.
    end;
  if y+h > actualheight then h := actualheight-y; // Trim line if it overflows.
  if y < 0 then // Check if position is negative.
    begin
    h := h+y; // Trim line.
    y := 0; // Align to edge.
    end;
  if (x > actualwidth) or (x < 0) then
    begin
    h := 0;
    x := 0;
    end;
  p := (y*scanwidth)+(x shl 2); // Find address for pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to (h-1) do
      begin
      pixelarray[p] := b; // Write pixel data.
      pixelarray[p+1] := g;
      pixelarray[p+2] := r;
      pixelarray[p+3] := 255;
      p := p+scanwidth; // Jump to next scanline, same y position.
      end
  else
    for i := 0 to (h-1) do
      begin
      pixelarray[p] := AlphaBlend(b, a, pixelarray[p]); // Write pixel data.
      pixelarray[p+1] := AlphaBlend(g, a, pixelarray[p+1]);
      pixelarray[p+2] := AlphaBlend(r, a, pixelarray[p+2]);
      pixelarray[p+3] := 255;
      p := p+scanwidth; // Jump to next scanline, same y position.
      end;
end;

procedure TForm1.DrawRect(r, g, b, a: byte; x, y, w, h: integer); // Draw solid rectangle.
var p, i, j: integer;
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
  if x+w > actualwidth then w := actualwidth-x; // Trim rectangle if it overflows.
  if y+h > actualheight then h := actualheight-y;  
  p := (y*scanwidth)+(x shl 2); // Find address for 1st pixel.
  if a = 255 then // Check alpha value (255 is opaque).
    for i := 0 to (h-1) do
      begin
      for j := 0 to (w-1) do
        begin
        pixelarray[p+(j shl 2)] := b; // Write pixel data.
        pixelarray[p+1+(j shl 2)] := g;
        pixelarray[p+2+(j shl 2)] := r;
        pixelarray[p+3+(j shl 2)] := 255;
        end;
      p := p+scanwidth; // Jump to next scanline, same y position.
      end
  else
    for i := 0 to (h-1) do
      begin
      for j := 0 to (w-1) do
        begin
        pixelarray[p+(j shl 2)] := AlphaBlend(b, a, pixelarray[p+(j shl 2)]); // Write pixel data.
        pixelarray[p+1+(j shl 2)] := AlphaBlend(g, a, pixelarray[p+1+(j shl 2)]);
        pixelarray[p+2+(j shl 2)] := AlphaBlend(r, a, pixelarray[p+2+(j shl 2)]);
        pixelarray[p+3+(j shl 2)] := 255;
        end;
      p := p+scanwidth; // Jump to next scanline, same y position.
      end;
end;

procedure TForm1.DrawBox(r, g, b, a: byte; x, y, w, h: integer); // Draw empty box.
begin
  DrawHLine(r, g, b, a, x, y, w); // Top.
  DrawVLine(r, g, b, a, x, y+1, h-2); // Left.
  DrawVLine(r, g, b, a, x+w-1, y+1, h-2); // Right.
  DrawHline(r, g, b, a, x, y+h-1, w); // Bottom.
end;

procedure TForm1.DrawBoxFill(r, g, b, a, r2, g2, b2, a2: byte; x, y, w,
  h: integer); // Draw filled box.
begin
  DrawBox(r, g, b, a, x, y, w, h); // Draw box.
  DrawRect(r2, g2, b2, a2, x+1, y+1, w-2, h-2); // Fill it.
end;
    
procedure TForm1.DrawBox2(r, g, b, a: byte; x, y, w, h, t: integer); // Draw empty box with thicker lines.
var i: integer;
begin
  for i:= 0 to (t-1) do
    DrawBox(r,g,b,a,x+i,y+i,w-(i shl 1),h-(i shl 1)); // Draw concentric boxes.
end;

procedure TForm1.DrawBoxFill2(r, g, b, a, r2, g2, b2, a2: byte; x, y, w,
  h, t: integer); // Draw filled box with thicker lines.
begin
  DrawBox2(r, g, b, a, x, y, w, h, t); // Draw box.
  DrawRect(r2, g2, b2, a2, x+t, y+t, w-(t shl 1), h-(t shl 1)); // Fill it.
end;

procedure TForm1.FillScreen(r, g, b: byte); // Fill screen with one colour.
var i: integer;
begin
  DrawHLine(r, g, b, 255, 0, 0, visiblewidth); // Fill first visible line.
  for i := 1 to (visibleheight-1) do // Copy visible lines.
    Move(pixelarray[0],pixelarray[i*scanwidth],(visiblewidth shl 2));
end;

end.
