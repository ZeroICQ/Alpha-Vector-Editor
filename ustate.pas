unit UState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TAppState = (apsNewFile, apsNotModified, apsModified);
  TFileState = (fisNotSaved, fisSaved);

  procedure SetAppState(AAppState: TAppState);
  procedure SetAppStateNewFile;
  procedure SetAppStateModified;
  procedure SetAppStateNotModified;
  procedure InitSaveLoad(AForm: TForm);
  function GetAppState: TAppState;
  function GetFileState: TFileState;
  function GetFilePath: String;
implementation

var
  AppState: TAppState;
  AppForm: TForm;
  AppName: String;
  FilePath: String;
  FileState: TFileState;

procedure SetAppState(AAppState: TAppState);
begin
  AppState := AAppState;
end;

procedure SetAppStateNewFile;
begin
  AppState := apsNewFile;
  FilePath := 'New Image';
  AppForm.Caption := FilePath + ' - ' + AppName;
  FileState := fisNotSaved;
end;

procedure SetAppStateModified;
begin
  AppState := apsModified;
  AppForm.Caption := '*' + FilePath + ' - ' + AppName;
end;

procedure SetAppStateNotModified;
begin
  AppState := apsNotModified;
  AppForm.Caption := FilePath;
end;

procedure InitSaveLoad(AForm: TForm);
begin
  AppForm := AForm;
  AppName := AForm.Caption;
  SetAppStateNewFile;
end;

function GetAppState: TAppState;
begin
  Result := AppState;
end;

function GetFileState: TFileState;
begin
  Result := FileState;
end;

function GetFilePath: String;
begin
  Result := FilePath;
end;

end.

