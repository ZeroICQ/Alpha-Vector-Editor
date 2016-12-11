unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UTransform, StdCtrls,
  ExtCtrls, Spin, UParameters, FPCanvas;

type

  TSelectionMode = (Select, Add);
  TIntArr = array of Integer;

  { TTool }

  TTool = class
  private
    FFigure: TFigure;
    FPanel: TPanel;
    FIcon: String;
    procedure InitParams; virtual; abstract;
  public
    property Icon: String read FIcon write FIcon;
    procedure Init(APanel: TPanel);
    procedure AddParam(AParam: TParameter);
    function GetFigure: TFigure;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); virtual; abstract;
    procedure MouseMove(AMousePos: TPoint); virtual; abstract;
    procedure MouseUp(AMousePos: TPoint); virtual;
  end;

  { THandTool }

  THandTool = class(TTool)
  private
    FStartingPoint: TDoublePoint;
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TMoveTool }

  TMoveTool = class(THandTool)
  public
    constructor Create;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TSelectionTool }

  TSelectionTool = class(TTool)
  private
    FStartingPoint: TDoublePoint;
    FShift: TShiftState;
    procedure CrossParams(AParams: TParams);
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
     procedure MouseMove(AMousePos: TPoint); override;
     procedure MouseUp(AMousePos: TPoint); override;
     procedure SelectFigures(ADoubleRect: TDoubleRect);
     procedure SelectFigure(ADoublePoint: TDoublePoint; AMode: TSelectionMode);
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
  private
    FStartingPoint: TDoublePoint;
    FMouseButton: TMouseButton;
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
    procedure MouseMove(AMousePos: TPoint); override;
    procedure MouseUp(AMousePos: TPoint); override;
  end;

  { TFigureTool }

  TFigureTool = class(TTool)
  private
    FLineWidth: Integer;
    FLineStyle: TFPPenStyle;
    procedure InitParams; override;
    procedure ChangeLineWidth(AWidth: Integer);
    procedure ChangeLineStyle(ALineStyle: TFPPenStyle);
  end;

  { TTwoPointFigureTool }

  TTwoPointFigureTool = class(TFigureTool)
  public
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TFilledFigureTool }

  TFilledFigureTool = class(TTwoPointFigureTool)
  private
    FBrushStyle: TFPBrushStyle;
    procedure ChangeBrushStyle(ABrushStyle: TFPBrushStyle);
    procedure InitParams; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TFigureTool)
  private
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TFilledFigureTool)
  private
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
  end;

  { TRoundRectangleTool }

  TRoundRectangleTool = class(TFilledFigureTool)
  private
    FFactorX: Integer;
    FFactorY: Integer;
    procedure InitParams; override;
    procedure ChangeXFactor(AFactor: Integer);
    procedure ChangeYFactor(AFactor: Integer);
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
  end;

  { TLineTool }

  TLineTool = class(TTwoPointFigureTool)
  private
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFilledFigureTool)
  private
    procedure InitParams; override;
  public
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
  end;

  { TRegularPolygonTool }

  TRegularPolygonTool = class(TFilledFigureTool)
  private
    FCorners: Integer;
    procedure InitParams; override;
    procedure ChangeCornersNumber(ACorners: Integer);
  public
    constructor Create;
    procedure MouseMove(AMousePos: TPoint); override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton; AShift: TShiftState); override;
  end;

var
  Tools: array of TTool;

implementation

{ Misc }

function IsInArray(i: Integer; AArr: TIntArr): Boolean;
var j: Integer;
begin
  for j := Low(AArr) to High(AArr) do begin
    if i = AArr[j] then Exit(True);
  end;
  Result := False;
end;

procedure DeselectAllFigures;
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do Figures[i].IsSelected := False;
end;

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

{ TMoveTool }

constructor TMoveTool.Create;
begin
  Inherited;
  FIcon := 'img/move.bmp';
end;

procedure TMoveTool.MouseMove(AMousePos: TPoint);
var
  i: Integer;
  Displacement: TDoublePoint;
begin
  Displacement := (DispToWorldCoord(AMousePos) - FStartingPoint);
  FStartingPoint := DispToWorldCoord(AMousePos);

  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      Figures[i].Move(Displacement);
  end;
end;

{ TSelectionTool }

constructor TSelectionTool.Create;
begin
  Inherited;
  FIcon := 'img/selection.bmp';
end;

procedure TSelectionTool.CrossParams(AParams: TParams);
var i, j: Integer;
  IsFound: Boolean;
  IndexesToDelete: array of Integer;
  TempParams: TParams;
begin
  if Length(FigureCommonParams) = 0 then begin
    FigureCommonParams := AParams;
    Exit;
  end;
  //ищем что удалить
  for i := Low(FigureCommonParams) to High(FigureCommonParams) do begin
    for j := Low(AParams) to High(AParams) do begin
      IsFound := False;
      if FigureCommonParams[i].ClassType = AParams[j].ClassType then begin
        IsFound := True;
        if not (FigureCommonParams[i].GetValue = AParams[j].GetValue) then
          FigureCommonParams[i].SetEmpty;
          Break;
      end;
    end;
    if not IsFound then begin
      SetLength(IndexesToDelete, Length(IndexesToDelete) + 1);
      IndexesToDelete[High(IndexesToDelete)] := i;
    end;
  end;
  //удаляем
  SetLength(TempParams, Length(FigureCommonParams) - Length(IndexesToDelete));
  j := Low(TempParams);
  for i := Low(FigureCommonParams) to High(FigureCommonParams) do begin
    if IsInArray(i, IndexesToDelete) then Continue;
    TempParams[j] := FigureCommonParams[i];
    j += 1;
  end;
  FigureCommonParams := TempParams;
end;

procedure TSelectionTool.InitParams;
var
  i: Integer;
begin
  //поиск праметров у фигур
  for i := Low(FigureCommonParams) to High(FigureCommonParams) do begin
    FigureCommonParams[i].Hide;
  end;
  FigureCommonParams := Nil;

  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then begin
      CrossParams(Figures[i].GetParams);
    end;
  end;
  ShowParams(FigureCommonParams, FPanel);
end;

procedure TSelectionTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FShift := AShift;
  FFigure := TSelection.Create(DispToWorldCoord(AMousePos));
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure TSelectionTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TSelection).SetSecondPoint(DispToWorldCoord(AMousePos));
end;

procedure TSelectionTool.MouseUp(AMousePos: TPoint);
const
  Delta = 3;//px
var
  SelectionBounds: TDoubleRect;
  SelectionWidth, SelectionHeight: Double;
  SelectionMode: TSelectionMode;
begin
  {TODO: вынести в методы фигур}
  SelectionBounds := FFigure.GetBounds;
  SelectionWidth := SelectionBounds.Right - SelectionBounds.Left;
  SelectionHeight := SelectionBounds.Bottom - SelectionBounds.Top;

  if (SelectionWidth > Delta / Scale) or
    (SelectionHeight > Delta / Scale)
  then begin
    DeselectAllFigures;
    SelectFigures(SelectionBounds);
  end
  else begin
    if (ssCtrl in FShift) then SelectionMode := Add
    else begin
      DeselectAllFigures;
      SelectionMode := Select;
    end;
    SelectFigure(FStartingPoint, SelectionMode);
  end;
  FreeAndNil(FFigure);
  Init(FPanel);
end;

procedure TSelectionTool.SelectFigures(ADoubleRect: TDoubleRect);
var i: Integer;
begin
  for i := High(Figures) downto Low(Figures) do begin
    with Figures[i] do begin
      if IsIntersect(ADoubleRect) then IsSelected := True;
    end;
  end;
end;

procedure TSelectionTool.SelectFigure(ADoublePoint: TDoublePoint;
  AMode: TSelectionMode);
var i: Integer;
begin
  for i := High(Figures) downto Low(Figures) do begin
    with Figures[i] do begin
      if IsPointInside(ADoublePoint) then begin
        if AMode = Add then IsSelected := not IsSelected
        else IsSelected := True;
        Exit;
      end;
    end;
  end;
end;

{ TRoundRectangleTool }

constructor TRoundRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/roundrectangle.bmp';
end;

procedure TRoundRectangleTool.InitParams;
begin
  Inherited;
  FFactorX := 30;
  FFactorY := 30;
  AddParam(TFactorParameter.Create('Скругление по X: ', @ChangeXFactor, FFactorX));
  AddParam(TFactorParameter.Create('Скругление по Y: ', @ChangeYFactor, FFactorY));

end;

procedure TRoundRectangleTool.ChangeXFactor(AFactor: Integer);
begin
  FFactorX := AFactor;
end;

procedure TRoundRectangleTool.ChangeYFactor(AFactor: Integer);
begin
  FFactorY := AFactor;
end;

procedure TRoundRectangleTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FFigure := TRoundRectangle.Create(DispToWorldCoord(AMousePos), APenColor,
  ABrushColor, FLineStyle, FLineWidth, FBrushStyle, FFactorX, FFactorY);
end;

{ TRegularPolygonTool }

constructor TRegularPolygonTool.Create;
begin
  Inherited;
  FIcon := 'img/polygon.bmp';
end;

procedure TRegularPolygonTool.InitParams;
begin
  Inherited;
  FCorners := 3;
  AddParam(TCornersNumberParameter.Create(@ChangeCornersNumber, FCorners));
end;

procedure TRegularPolygonTool.ChangeCornersNumber(ACorners: Integer);
begin
  FCorners := ACorners;
end;

procedure TRegularPolygonTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TRegularPolygon).SetSecondPoint(DispToWorldCoord(AMousePos));
end;

procedure TRegularPolygonTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FFigure := TRegularPolygon.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, FLineStyle, FLineWidth,
    FBrushStyle, FCorners);
end;

{ TFilledFigureTool }

procedure TFilledFigureTool.ChangeBrushStyle(ABrushStyle: TFPBrushStyle);
begin
  FBrushStyle := ABrushStyle;
end;

procedure TFilledFigureTool.InitParams;
begin
  Inherited;
  FBrushStyle := bsSolid;
  AddParam(TBrushStyleParameter.Create(@ChangeBrushStyle, FBrushStyle));
end;

{ TFigureTool }
procedure TFigureTool.InitParams;
begin
  FLineWidth := 3;
  FLineStyle := psSolid;
  AddParam(TLineWidthParameter.Create(@ChangeLineWidth, FLineWidth));
  AddParam(TLineStyleParameter.Create(@ChangeLineStyle, FLineStyle));
end;

procedure TFigureTool.ChangeLineWidth(AWidth: Integer);
begin
  FLineWidth := AWidth;
end;

procedure TFigureTool.ChangeLineStyle(ALineStyle: TFPPenStyle);
begin
  FLineStyle := ALineStyle;
end;

{ TMagnifierTool }
constructor TMagnifierTool.Create;
begin
  Inherited;
  FIcon := 'img/magnifier.bmp';
end;

procedure TMagnifierTool.InitParams;
begin
  //пока ничего
end;

procedure TMagnifierTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FMouseButton := AButton;
  FFigure := TSelection.Create(DispToWorldCoord(AMousePos));
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure TMagnifierTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TSelection).SetSecondPoint(DispToWorldCoord(AMousePos));
end;

procedure TMagnifierTool.MouseUp(AMousePos: TPoint);
const
  Delta = 5;//px
var
  XScale, YScale: Double;
  SelectionBounds: TDoubleRect;
  SelectionWidth, SelectionHeight: Double;
begin
  SelectionBounds := FFigure.GetBounds;
  SelectionWidth := SelectionBounds.Right - SelectionBounds.Left;
  SelectionHeight := SelectionBounds.Bottom - SelectionBounds.Top;

  if (SelectionWidth > Delta / Scale) and
    (SelectionHeight > Delta / Scale)
  then begin
    XScale := (DispDimensions.Width) / SelectionWidth;
    YScale := (DispDimensions.Height) / SelectionHeight;
    Scale := Min(XScale, YScale);
    //Размещение по центру
    SetCanvasOffset(
      SelectionBounds.Left * Scale - (DispDimensions.Width - (SelectionWidth ) * Scale) / 2,
      SelectionBounds.Top * Scale - (DispDimensions.Height - (SelectionHeight) * Scale) / 2);
  end
  else begin
    if FMouseButton = mbLeft then IncreaseScale;
    if FMouseButton = mbRight then DecreaseScale;
    AddCanvasOffset((FStartingPoint - DispToWorldCoord(AMousePos)) * Scale);
  end;
  FreeAndNil(FFigure);
end;

{ THandTool }
constructor THandTool.Create;
begin
  Inherited;
  FIcon := 'img/hand.bmp';
end;

procedure THandTool.InitParams;
begin
  //нет параметров
end;

procedure THandTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure THandTool.MouseMove(AMousePos: TPoint);
begin
  AddCanvasOffset((FStartingPoint - DispToWorldCoord(AMousePos)) * Scale);
end;

{ TTool }

procedure TTool.AddParam(AParam: TParameter);
begin
  SetLength(Params, Length(Params) + 1);
  Params[High(Params)] := AParam;
end;

procedure TTool.Init(APanel: TPanel);
var i: Integer;
begin
  for i := Low(FigureCommonParams) to High(FigureCommonParams) do begin
    FigureCommonParams[i].Hide;
  end;

  for i := Low(Params) to High(Params) do begin
    Params[i].Free;
  end;
  Params := Nil;
  FPanel := APanel;
  InitParams;
  ShowParams(Params, APanel);;
end;

function TTool.GetFigure: TFigure;
begin
  Result := FFigure;
end;

procedure TTool.MouseUp(AMousePos: TPoint);
begin
  //Ничего не делать
end;

{ TTwoPointFigureTool }
procedure TTwoPointFigureTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TTwoPointFigure).SetSecondPoint(DispToWorldCoord(AMousePos));
end;

{ TPolylineTool }
constructor TPolylineTool.Create;
begin
  Inherited;
  FIcon := 'img/polyline.bmp';
end;

procedure TPolylineTool.InitParams;
begin
  Inherited;
end;

procedure TPolylineTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FFigure := TPolyline.Create(
    DispToWorldCoord(AMousePos), APenColor, FLineStyle, FLineWidth);
end;

procedure TPolylineTool.MouseMove(AMousePos: TPoint);
begin
  (FFigure as TPolyline).AddPoint(DispToWorldCoord(AMousePos));
end;

{ TRectangleTool }
constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'img/rectangle.bmp';
end;

procedure TRectangleTool.InitParams;
begin
  Inherited;
end;

procedure TRectangleTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FFigure := TRectangle.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle);
end;

{ TEllipseTool }
constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'img/ellipse.bmp';
end;

procedure TEllipseTool.InitParams;
begin
  inherited;
end;

procedure TEllipseTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FFigure := TEllipse.Create(
    DispToWorldCoord(AMousePos), APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle);
end;

{ TLineTool }
constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'img/line.bmp';
end;

procedure TLineTool.InitParams;
begin
  inherited;
end;

procedure TLineTool.MouseDown(AMousePos: TPoint; APenColor,
  ABrushColor: TColor; AButton: TMouseButton; AShift: TShiftState);
begin
  FFigure := TLine.Create(
   DispToWorldCoord(AMousePos), APenColor, FLineStyle, FLineWidth);
end;

initialization

RegisterTool(THandTool.Create);
RegisterTool(TMoveTool.Create);
RegisterTool(TMagnifierTool.Create);
RegisterTool(TSelectionTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TRoundRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
RegisterTool(TRegularPolygonTool.Create);

end.

