unit Generic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, ScanLineFunc, pngimage;

type
  TForm1 = class(TForm)
    imgMain: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitImage(Form1,imgMain); // Set image width & height to match form.
  FillScreen(0,0,0);
  LoadSheet('test.png');
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  MatchWindow; // Set boundaries to match window.
  FillScreen(255,128,0);
  DrawBoxFill2(0,255,0,255,255,0,0,128,0,0,50,50,3);
  DrawBoxFill2(0,255,0,128,0,0,255,192,visiblewidth-50,visibleheight-50,50,50,2);
  DrawPNG(0,0,80,80,0,100,2,4,0,255,255,255,255);
  DrawPNG(160,0,80,80,140,140,1,1,3,255,255,255,255);
  DrawPNG(0,0,80,80,200,200,1,1,1,255,255,255,255);
  DrawPNG(80,0,80,80,24,240,-1,-1,2,255,0,255,255);
  DrawPNG(0,0,80,80,400,400,-2,-2,0,255,255,255,128);
  DrawWholePNG(600,200,1,1,3,128,255,255,255);
  DrawLine(0,0,0,100,200,20,220,200);
  DrawLine(0,255,255,255,500,500,100,1);
  DrawLine(255,255,255,255,0,0,visiblewidth-50,visibleheight-50);
  DrawTriangleFlat(255,255,255,128,80,0,10,160,160,0);
  DrawTriangleFlat(255,255,255,128,400,180,480,520,0,0);
  DrawTriangle(0,255,0,128,250,0,0,250,500,500);
end;

end.
