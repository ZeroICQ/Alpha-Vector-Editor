{ Формат сохранениея состоит из 3 строк:
  Класс фигуры
  Координаты [x]:[y];[x]:[y]...
  Параметр:Значение;Параметр:Значение...}
unit USaveLoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, UAppState, typinfo, strutils;
type
  TExec = function: String of Object;

  procedure SaveFile(APath: String; AFigures: array of TFigure);
  function FileLoad(APath: String; var AFigures: TFigureArr): Boolean;

implementation
const
  FILE_SIGNATURE = '--VECTORGRAPHICSEDITOR--SAVEDIMAGE--';
  TypeKinds: TTypeKinds = tkAny;

function GetAllProps(AInstance: TObject; var APropList: PPropList): Integer;
var
  PropCount: Integer;
begin
  PropCount := GetPropList(AInstance.ClassInfo, TypeKinds, Nil);
  GetMem(APropList, PropCount * SizeOf(PPropInfo));
  GetPropList(AInstance.ClassInfo, TypeKinds, APropList);
  Result := PropCount;
end;

function CreateFigure(AClass: String; ACoords, AParams: TTwoDStrArr):  TFigure;
var
  i: Integer;
  Figure: TFigure;
  FigureClass: TClass;
  PropList: PPropList;
  PropCount: Integer;
  PropValue: String;
  PropKind: TTypeKind;
  ParamProp: TObject;
begin
  case AClass of
    'TRectangle':      Figure := TRectangle.CreateEmptyProps(ACoords);
    'TRoundRectangle': Figure := TRoundRectangle.CreateEmtpyProps(ACoords);
    'TLine':           Figure := TLine.CreateEmtpyProps(ACoords);
    'TPolyline':       Figure := TPolyline.CreateEmtpyProps(ACoords);
    'TEllipse':        Figure := TEllipse.CreateEmptyProps(ACoords);
    'TRegularPolygon': Figure := TRegularPolygon.CreateEmtpyProps(ACoords);
  end;

  //(Figure as TFigure).SetCoord(ACoords);
  for i := Low(AParams) to High(AParams) do begin
    PropKind := GetPropInfo(Figure, AParams[i, 0])^.PropType^.Kind;
    case PropKind of
      tkInteger: SetInt64Prop(Figure, AParams[i, 0], StrToInt64(AParams[i, 1]));
      tkClass: begin
        ParamProp := GetObjectProp(Figure, AParams[i,0]);
        SetPropValue(ParamProp, 'Value', AParams[i, 1]);
      end;
    end;
  end;
  Result := Figure;
end;

function SplitParams(AStr: String): TTwoDStrArr;
var
  SplitedParams: TTwoDStrArr;
  i: Integer;
  Param: String;
begin
  for i := 1 To WordCount(AStr, [';']) do begin
    Param := ExtractDelimited(i, AStr, [';']);
    SetLength(SplitedParams, Length(SplitedParams) + 1);
    SetLength(SplitedParams[i - 1], 2);
    SplitedParams[i - 1, 0] := ExtractDelimited(1, Param, [':']);
    SplitedParams[i - 1, 1] := ExtractDelimited(2, Param, [':']);
  end;

  Result := SplitedParams;
end;

procedure SaveFile(APath: String; AFigures: array of TFigure);
var
  OutFile: TextFile;
  i, j: Integer;
  PropList: PPropList;
  PropCount: Integer;
  PropValue: String;
  PropKind: TTypeKind;
  PropParampKind: TTypeKind;
  ParamProp: TObject;
begin
  try
    Assign(OutFile, APath);
    Rewrite(OutFile);
    WriteLn(OutFile, FILE_SIGNATURE);

    for i := Low(AFigures) to High(AFigures) do begin
      PropCount := GetAllProps(AFigures[i], PropList);
      writeln(OutFile, AFigures[i].ClassName);
      writeln(OutFile, AFigures[i].GetStrCoord);
      for j := 0 to PropCount - 1 do begin
        PropKind := PropList^[j]^.PropType^.Kind;

        case PropKind of
          tkInteger: PropValue := IntToStr(GetInt64Prop(AFigures[i], PropList^[j]^.Name));
          tkClass: begin
            ParamProp := GetObjectProp(AFigures[i], PropList^[j]^.Name);
            PropValue := GetPropValue(ParamProp, 'Value');
          end;
        end;
        write(OutFile, PropList^[j]^.Name +':'+ PropValue + ';');
      end;
      Freemem(PropList);
      writeln(OutFile);
    end;
  finally
    Close(OutFile);
  end;
  SetFilePath(APath);
  SetAppStateNotModified;
  SetFileStateSaved;
end;

function FileLoad(APath: String; var AFigures: TFigureArr): Boolean;
var
  InpFile: TextFile;
  Line: String;
  FigureClass, FigureCoords, FigureStrParams: String;
  SplitedParams: TTwoDStrArr;
  SplitedCoords: TTwoDStrArr;
  i: Integer;
begin
  for i := Low(AFigures) to High(AFigures) do
    FreeAndNil(AFigures[i]);
  AFigures := nil;
  try
    Assign(InpFile, APath);
    Reset(InpFile);
    ReadLn(InpFile, Line);
    if Line <> FILE_SIGNATURE then
      Exit(False);

    while not EOF(InpFile) do begin
      Readln(InpFile, FigureClass);
      Readln(InpFile, FigureCoords);
      Readln(InpFile, FigureStrParams);
      SplitedCoords := SplitParams(FigureCoords);
      SplitedParams := SplitParams(FigureStrParams);
      SetLength(AFigures, Length(AFigures) + 1);
      AFigures[High(AFigures)] := CreateFigure(FigureClass, SplitedCoords, SplitedParams);
    end;

  finally
    Close(InpFile);
  end;
  SetFilePath(APath);
  SetAppStateNotModified;
  SetFileStateSaved;
  Exit(True);
end;

end.

