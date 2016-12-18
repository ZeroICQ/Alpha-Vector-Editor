{ Формат сохранениея состоит из 3 строк:
  Класс фигуры
  Координаты
  Параметр: Значение; Параметр: Значение...}
unit USave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, typinfo;
type
  TExec = function: String of Object;

  procedure SaveFile(APath: String; AFigures: array of TFigure);

implementation

procedure SaveFile(APath: String; AFigures: array of TFigure);
var
  OutFile: TextFile;
  i, j: Integer;
  PropList: PPropList;
  PropCount: Integer;
  PropValue: String;
  PropKind: TTypeKind;
  Exec: TExec;
  Routine : TMethod;
const
  TypeKinds: TTypeKinds = tkAny;
begin
  try
    Assign(OutFile, APath);
    Rewrite(OutFile);
    WriteLn(OutFile, '--VECTORGRAPHICSEDITOR--SAVEDIMAGE--');

    for i := Low(AFigures) to High(AFigures) do begin
      //оберунуть в  try , freemem для proplist
      PropCount := GetPropList(AFigures[i].ClassInfo, TypeKinds, Nil);
      GetMem(PropList, PropCount * SizeOf(PPropInfo));
      GetPropList(AFigures[i].ClassInfo, TypeKinds, PropList);
      //класс фигуры
      writeln(OutFile, AFigures[i].ClassName + ';');
      writeln(OutFile, AFigures[i].GetStrCoord);
      for j := 0 to PropCount - 1 do begin
        PropKind := PropList^[j]^.PropType^.Kind;

        case PropKind of
          tkInteger: PropValue := IntToStr(GetInt64Prop(AFigures[i], PropList^[j]^.Name));

          tkClass: begin
            Routine.Data := Pointer(GetObjectProp(AFigures[i], PropList^[j]^.Name));
            Routine.Code := GetObjectProp(AFigures[i], PropList^[j]^.Name).MethodAddress('GetStrValue');
            if not Assigned(Routine.Code) then begin
              Raise Exception.Create('GetStr Value method not found');
            end;
            Exec := TExec(Routine);
            PropValue := Exec();;
          end;

          else
            PropValue := 'unkown *'+IntToStr(ord(PropList^[j]^.PropType^.Kind));
        end;
        write(OutFile, PropList^[j]^.Name +': '+ PropValue + '; ');
      end;
      Freemem(PropList);
      writeln(OutFile);
    end;
  finally
    close(OutFile);
    //Freemem(PropList);
  end;
end;

end.

