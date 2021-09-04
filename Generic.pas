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
    procedure LoadFile(openthis: string);
    function GetWord(a: integer): word;
    function GetDword(a: integer): longword;
    function GetWordR(a: integer): word;
    function GetDwordR(a: integer): longword;
    function GetString(a, maxlength: integer): string;
    function GetBit(i, b: integer): byte;
    function GetNibble(i, n: integer): byte;
    function Explode(s, d: string; n: integer): string;
    function DoSum(s: string): int64;
    function Solve(s: string): int64;
    function Solve2(s, t: string): int64;
    function CRCString(s: string): longword;
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
    procedure FillScreen(r, g, b, a: byte; all: boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  myfile: file;
  filearray: array of byte;
  pixelarray: PByteArray;
  scanwidth, actualwidth, actualheight, visiblewidth, visibleheight: integer;
  readpixel: array[0..3] of byte;
  crctable: array[0..255] of longint;

implementation

{$R *.dfm}

{ File stuff }

procedure TForm1.LoadFile(openthis: string); // Open file and copy to array.
begin
  if FileExists(openthis) = true then
    begin
    AssignFile(myfile,openthis); // Get file.
    FileMode := fmOpenRead;
    Reset(myfile,1); // Read only.
    SetLength(filearray,FileSize(myfile));
    BlockRead(myfile,filearray[0],FileSize(myfile)); // Copy file to memory.
    CloseFile(myfile); // Close file.
    end;
end;

function TForm1.GetWord(a: integer): word; // Get word from file array.
begin
  result := (filearray[a]*$100)+filearray[a+1];
end;

function TForm1.GetDword(a: integer): longword; // Get longword form file array.
begin
  result := (GetWord(a)*$10000)+GetWord(a+2);
end;

function TForm1.GetWordR(a: integer): word; // Get reversed word.
begin
  result := (filearray[a+1]*$100)+filearray[a];
end;

function TForm1.GetDwordR(a: integer): longword; // Get reversed longword.
begin
  result := (GetWord(a+2)*$10000)+GetWord(a);
end;

function TForm1.GetString(a, maxlength: integer): string;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    if filearray[a] in [32..126] then
      result := result+Chr(filearray[a]) // Add character to string if valid.
    else maxlength := 0; // Otherwise end the string.
    Inc(a); // Next character.
    end;
end;

{ Byte operations }

function TForm1.GetBit(i, b: integer): byte; // Get bit from integer (output 0 or 1).
begin
  result := (i and (1 shl b)) shr b;
end;

function TForm1.GetNibble(i, n: integer): byte; // Get nibble from integer.
begin
  result := (i shr (n shl 2)) and $f;
end;

{ String operations }

function TForm1.Explode(s, d: string; n: integer): string; // Get part of a string using delimiter.
var n2: integer;
begin
  if (AnsiPos(d,s) = 0) and ((n = 0) or (n = -1)) then result := s // Output full string if delimiter not found.
  else
    begin
    if n > -1 then // Check for negative substring.
      begin
      s := s+d;
      n2 := n;
      end
    else
      begin
      d := AnsiReverseString(d);
      s := AnsiReverseString(s)+d; // Reverse string for negative.
      n2 := (n*-1)-1;
      end;
    while n2 > 0 do
      begin
      Delete(s,1,AnsiPos(d,s)+Length(d)-1); // Trim earlier substrings and delimiters.
      dec(n2);
      end;
    Delete(s,AnsiPos(d,s),Length(s)-AnsiPos(d,s)+1); // Trim later substrings and delimiters.
    if n < 0 then s := AnsiReverseString(s); // Un-reverse string if negative.
    result := s;
  end;
end;

function TForm1.DoSum(s: string): int64; // Convert a string sum (e.g. '1+1') to integer.
var i, r: int64;
  sub: string;
begin
  s := ReplaceStr(s,' ',''); // Strip spaces.
  s := ReplaceStr(s,'<<','shl'); // Replace << to avoid clash with <.
  s := ReplaceStr(s,'>>','shr'); // Replace >> to avoid clash with >.
  if AnsiPos('=',s) <> 0 then // Compare sides if string contains equal sign.
    if DoSum(Explode(s,'=',0)) = DoSum(Explode(s,'=',1)) then s := '1' // 1 for equal.
    else s := '0'; // 0 for different.
  if AnsiPos('<>',s) <> 0 then // Compare sides if string contains inequal sign.
    if DoSum(Explode(s,'<>',0)) <> DoSum(Explode(s,'<>',1)) then s := '1' // 1 for inequal.
    else s := '0'; // 0 for same.
  if AnsiPos('>',s) <> 0 then // Compare sides if string contains greater than sign.
    if DoSum(Explode(s,'>',0)) > DoSum(Explode(s,'>',1)) then s := '1' // 1 for greater than.
    else s := '0'; // 0 for less.
  if AnsiPos('<',s) <> 0 then // Compare sides if string contains less than sign.
    if DoSum(Explode(s,'<',0)) < DoSum(Explode(s,'<',1)) then s := '1' // 1 for less than.
    else s := '0'; // 0 for greater.
  s := ReplaceStr(s,'+','?+'); // Insert separator character.
  s := ReplaceStr(s,'-','?-');
  s := ReplaceStr(s,'*','?*');
  s := ReplaceStr(s,'/','?/');
  s := ReplaceStr(s,'&','?&'); // AND
  s := ReplaceStr(s,'and','?&');
  s := ReplaceStr(s,'^','?^'); // XOR
  s := ReplaceStr(s,'xor','?^');
  s := ReplaceStr(s,'|','?|'); // OR
  s := ReplaceStr(s,'or','?|');
  s := ReplaceStr(s,'mod','?mod');
  s := ReplaceStr(s,'shl','?shl'); // Shift left
  s := ReplaceStr(s,'shr','?shr'); // Shift right
  i := 0;
  r := StrtoInt64(Explode(s,'?',0));
  while Explode(s,'?',i) <> '' do
    begin
    sub := Explode(s,'?',i); // Get substring.
    if Copy(sub,1,1) = '+' then r := r+StrtoInt64(Explode(sub,'+',1)) // If +, add it.
    else if Copy(sub,1,1) = '-' then r := r-StrtoInt64(Explode(sub,'-',1))
    else if Copy(sub,1,1) = '*' then r := r*StrtoInt64(Explode(sub,'*',1))
    else if Copy(sub,1,1) = '/' then r := r div StrtoInt64(Explode(sub,'/',1))
    else if Copy(sub,1,1) = '&' then r := r and StrtoInt64(Explode(sub,'&',1))
    else if Copy(sub,1,1) = '|' then r := r or StrtoInt64(Explode(sub,'|',1))
    else if Copy(sub,1,1) = '^' then r := r xor StrtoInt64(Explode(sub,'^',1))
    else if Copy(sub,1,3) = 'mod' then r := r mod StrtoInt64(Explode(sub,'mod',1))
    else if Copy(sub,1,3) = 'shl' then r := r shl StrtoInt64(Explode(sub,'shl',1))
    else if Copy(sub,1,3) = 'shr' then r := r shr StrtoInt64(Explode(sub,'shr',1));
    inc(i);
    end;
  result := r;
end;

function TForm1.Solve(s: string): int64; // Convert sum with brackets to integer.
var sub, t: string;
begin    
  while AnsiPos('"',s) <> 0 do
    begin
    sub := Explode(s,'"',1); // Get contents of quotes.
    s := ReplaceStr(s,'"'+sub+'"','$'+InttoHex(CRCString(sub),8)); // Replace string with CRC32.
    end;
  while AnsiPos(']',s) <> 0 do
    begin
    sub := Explode(Explode(s,']',0),'[',-1); // Get contents of square brackets.
    t := AnsiRightStr(Explode(Explode(s,']',0),'[',-2),1); // Get character before bracket (b/w/d).
    s := ReplaceStr(s,t+'['+sub+']',InttoStr(Solve2(sub,t))); // Solve & remove brackets.
    end;
  while AnsiPos(')',s) <> 0 do
    begin
    sub := Explode(Explode(s,')',0),'(',-1); // Get contents of brackets.
    s := ReplaceStr(s,'('+sub+')',InttoStr(DoSum(sub))); // Solve & remove brackets.
    end;
  result := DoSum(s); // Final sum after brackets are gone.
end;

function TForm1.Solve2(s, t: string): int64; // Get data from file array.
var s1, s2: int64;
begin
  if t = 'b' then result := filearray[Solve(s)] // Return byte from file array.
  else if t = 'w' then result := GetWord(Solve(s)) // Return word.
  else if t = 'd' then result := GetDWord(Solve(s)) // Return longword.
  else if t = 's' then
    begin
    s1 := Solve(Explode(s,',',0));
    s2 := Solve(Explode(s,',',1));
    result := CRCString(GetString(s1,s2));
    end
  else result := 0; // Return nothing.
end;

function TForm1.CRCString(s: string): longword; // Get CRC32 of string.
var i, x: integer;   
  r: longint;
begin
  r := -1;
  for i := 1 to Length(s) do
    begin
    x := (Ord(s[i]) xor r) and $FF;
    r := (r shr 8) xor crctable[x];
    end;
  result := not r;
end;

{ Form management. }

procedure TForm1.FormCreate(Sender: TObject);
var i, j: integer;
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
  for i := 0 to 255 do // Create CRC32 lookup table.
    begin
    crctable[i] := i;
    for j := 0 to 7 do if Odd(crctable[i]) then
      crctable[i] := (crctable[i] shr 1) xor $EDB88320
      else crctable[i] := crctable[i] shr 1;
    end;
  DrawBoxFill2(0,192,0,255,255,0,255,128,450,0,200,200,3);
  DrawBox2(0,0,0,255,100,10,100,50,5);
  FillScreen(0,0,0,255,false);

  setlength(filearray,1);
  filearray[0] := $41;
  //showmessage(inttohex(Solve('((1+1)*1=2) and (s[0,1] = "b")'),1));
  showmessage(inttohex(Solve('1 <> 2'),1));
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  visiblewidth := Form1.ClientWidth; // Set boundaries to match window.
  visibleheight := Form1.ClientHeight;
  imgMain.Width := visiblewidth;
  imgMain.Height := visibleheight;
  FillScreen(0,0,0,255,false);
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

procedure TForm1.FillScreen(r, g, b, a: byte; all: boolean); // Fill screen with one colour.
var i: integer;
begin
  if all = true then // Fill entire bitmap.
    begin
    DrawHLine(r, g, b, a, 0, 0, actualwidth); // Fill first scanline.
    for i := 1 to (actualheight-1) do // Copy scanlines.
      Move(pixelarray[0],pixelarray[i*scanwidth],(actualwidth shl 2));
    end
  else // Fill only what's visible.
    begin
    DrawHLine(r, g, b, a, 0, 0, visiblewidth); // Fill first visible line.
    for i := 1 to (visibleheight-1) do // Copy visible lines.
      Move(pixelarray[0],pixelarray[i*scanwidth],(visiblewidth shl 2));
    end;
end;

end.
