program Project1;

uses
  Forms,
  Generic in 'Generic.pas' {Form1},
  ScanLineFunc in 'ScanLineFunc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
