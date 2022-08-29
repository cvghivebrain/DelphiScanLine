unit Generic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, ScanLineFunc;

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
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  MatchWindow; // Set boundaries to match window.
  FillScreen(0,0,0);
  DrawBoxFill2(0,255,0,255,255,0,0,128,0,0,50,50,3);
  DrawBoxFill2(0,255,0,128,0,0,255,192,visiblewidth-50,visibleheight-50,50,50,2);
end;

end.
