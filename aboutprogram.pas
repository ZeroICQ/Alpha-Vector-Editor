unit aboutprogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TaboutProgramForm }

  TaboutProgramForm = class(TForm)
    AuthorLabel: TLabel;
    GroupLabel: TLabel;
    YearLabel: TLabel;

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  aboutProgramForm: TaboutProgramForm;

implementation

{$R *.lfm}

{ TaboutProgramForm }


end.

