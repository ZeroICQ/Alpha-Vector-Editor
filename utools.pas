unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, Controls, SysUtils, UFigures, Graphics, UTransform, StdCtrls,
  ExtCtrls, Spin, UParameters, FPCanvas;

type

  { TTool }

  TTool = class
    FFigure: TFigure;
    FPanel: TPanel;
    FIcon: String;
    procedure Init(APanel: TPanel);
    procedure InitParams; virtual; abstract;
    procedure AddParam(AParam: TParameter);
    procedure ShowParams;
    function GetFigure: TFigure;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); virtual; abstract;
    procedure MouseMove(AMousePos: TPoint); virtual; abstract;
    procedure MouseUp(AMousePos: TPoint); virtual;
  end;

  { THandTool }

  THandTool = class(TTool)
    FStartingPoint: TDoublePoint;
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    FStartingPoint: TDoublePoint;
    FIsSelectingArea: Boolean;
    FMouseButton: TMouseButton;
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
    procedure MouseUp(AMousePos: TPoint); override;
  end;

  { TFigureTool }

  TFigureTool = class(TTool)
    FLineWidth: Integer;
    FLineStyle: TFPPenStyle;
    procedure InitParams; override;
    procedure ChangeLineWidth(AWidth: Integer);
    procedure ChangeLineStyle(ALineStyle: TFPPenStyle);
  end;

  { TTwoPointFigureTool }

  TTwoPointFigureTool = class(TFigureTool)
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TFilledFigureTool }

  TFilledFigureTool = class(TTwoPointFigureTool)
    FBrushStyle: TFPBrushStyle;
    procedure ChangeBrushStyle(ABrushStyle: TFPBrushStyle);
    procedure InitParams; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TFigureTool)
    procedure InitParams; override;
    constructor Create;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
    procedure MouseMove(AMousePos: TPoint); override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TFilledFigureTool)
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

  { TLineTool }

  TLineTool = class(TTwoPointFigureTool)
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFilledFigureTool)
    constructor Create;
    procedure InitParams; override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

  { TRegularPolygonTool }

  TRegularPolygonTool = class(TFilledFigureTool)
    FCorners: Integer;
    constructor Create;
    procedure InitParams; override;
    procedure ChangeCornersNumber(ACorners: Integer);
    procedure MouseMove(AMousePos: TPoint); override;
    procedure MouseDown(AMousePos: TPoint; APenColor, ABrushColor: TColor;
      AButton: TMouseButton); override;
  end;

var
  Tools: array of TTool;

implementation

var
  Params: array of TParameter;

{ Misc }

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
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
  AddParam(TCornersNumberParameter.Create(@ChangeCornersNumber));
  FCorners := 3;
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
  ABrushColor: TColor; AButton: TMouseButton);
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
  AddParam(TBrushStyleParameter.Create(@ChangeBrushStyle));
  FBrushStyle := bsSolid;
end;

{ TFigureTool }
procedure TFigureTool.InitParams;
begin
  AddParam(TLineWidthParameter.Create(@ChangeLineWidth));
  AddParam(TLineStyleParameter.Create(@ChangeLineStyle));
  FLineWidth := 3;
  FLineStyle := psSolid;
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
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FIsSelectingArea := False;
  FMouseButton := AButton;
  FFigure := TRectangleLine.Create(
  DispToWorldCoord(AMousePos), APenColor, ABrushColor, psSolid, 1, bsClear);
  FStartingPoint := DispToWorldCoord(AMousePos);
end;

procedure TMagnifierTool.MouseMove(AMousePos: TPoint);
begin
  FIsSelectingArea := True;
  (FFigure as TRectangleLine).SetSecondPoint(DispToWorldCoord(AMousePos));
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

  if FIsSelectingArea and
    (SelectionWidth > Delta / Scale) and
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
  FIsSelectingArea := False;
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
  ABrushColor: TColor; AButton: TMouseButton);
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

procedure TTool.ShowParams;
var i: Integer;
begin
  for i := 0 to High(Params) do begin
      with Params[i] do begin
        FLabel.Top := i * 50;
        FLabel.Left := 2;
        FLabel.Parent := FPanel;
        FComponent.Top := i * 50 + FLabel.ClientHeight + 5;
        FComponent.Left := FPanel.ClientWidth - FComponent.ClientWidth;
        FComponent.Parent := FPanel;
      end;
  end;
end;

procedure TTool.Init(APanel: TPanel);
var i: Integer;
begin
  for i := Low(Params) to High(Params) do begin
    Params[i].Free;
  end;
  Params := Nil;
  FPanel := APanel;
  InitParams;
  ShowParams;
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
  ABrushColor: TColor; AButton: TMouseButton);
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
  ABrushColor: TColor; AButton: TMouseButton);
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
  ABrushColor: TColor; AButton: TMouseButton);
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
  ABrushColor: TColor; AButton: TMouseButton);
begin
  FFigure := TLine.Create(
   DispToWorldCoord(AMousePos), APenColor, FLineStyle, FLineWidth);
end;

initialization

RegisterTool(THandTool.Create);
RegisterTool(TMagnifierTool.Create);
RegisterTool(TPolylineTool.Create);
RegisterTool(TRectangleTool.Create);
RegisterTool(TEllipseTool.Create);
RegisterTool(TLineTool.Create);
RegisterTool(TRegularPolygonTool.Create);

end.

