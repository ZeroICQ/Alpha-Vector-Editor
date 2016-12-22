unit UAppState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TFileState = (fisNotSaved, fisSaved);
  TTwoDStrArr = array of array of String;

  procedure SetFileStateSaved;
  procedure SetFileStateNotSaved;
  procedure UpdateCaption(AIsModified: Boolean);
  procedure InitAppState(AForm: TForm);
  function GetFileState: TFileState;
  function GetFilePath: String;
  procedure SetFilePath(APath: String);

implementation

var
  AppForm: TForm;
  AppName: String;
  FilePath: String;
  FileState: TFileState;

procedure SetAppStateNewFile;
begin
  FilePath := 'New Image';
  FileState := fisNotSaved;
end;

procedure SetFileStateSaved;
begin
  FileState := fisSaved;
end;

procedure SetFileStateNotSaved;
begin
  FileState := fisNotSaved;
end;

procedure UpdateCaption(AIsModified: Boolean);
var
  LCaption: String;
begin
  LCaption :=  FilePath + ' - ' + AppName;;
  If AIsModified then LCaption := '*' + LCaption;
  AppForm.Caption := LCaption;
end;

procedure InitAppState(AForm: TForm);
begin
  AppForm := AForm;
  AppName := AForm.Caption;
  SetAppStateNewFile;
end;

function GetFileState: TFileState;
begin
  Result := FileState;
end;

function GetFilePath: String;
begin
  Result := FilePath;
end;

procedure SetFilePath(APath: String);
begin
  FilePath := APath;
end;

end.

