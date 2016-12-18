program VectorGraphicsEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, aboutprogram;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TVectorEditor, VectorEditor);
  Application.CreateForm(TaboutProgramForm, aboutProgramForm);
  Application.Run;
end.

