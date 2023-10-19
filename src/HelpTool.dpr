program HelpTool;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  Unit3 in 'Unit3.pas' {AboutForm},
  Unit1 in 'Unit1.pas',
  Unit4 in 'Unit4.pas' {PrintPreviewerFrame: TFrame},
  Unit5 in 'Unit5.pas' {Frame5: TFrame},
  Scanner in 'Scanner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
