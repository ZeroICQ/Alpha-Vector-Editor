unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  UTransform, Classes, SysUtils, Graphics, math, FPCanvas;

type

  { TFigure }

  TFigure = class
    PenColor: TColor;
    PenStyle: TFPPenStyle;
    Thickness: Integer;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
    procedure Draw(Canvas: TCanvas); virtual;
    procedure DrawFigure(Canvas: TCanvas); virtual; abstract;
    function GetBounds: TDoubleRect; virtual; abstract;
  end;

  { TPolyline }

  TPolyline = class(TFigure)
    Vertexes: array of TDoublePoint;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer);
    procedure AddPoint(ADoublePoint: TDoublePoint);
    procedure DrawFigure(Canvas: TCanvas); override;
    function GetBounds: TDoubleRect; override;
  end;

  { TTwoPointFigure }

  TTwoPointFigure = class(TFigure)
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); virtual; abstract;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle;
      AThickness: Integer);
  end;

  { TFilledFigure }

  TFilledFigure = class(TTwoPointFigure)
    FBrushColor: TColor;
    FBrushStyle: TFPBrushStyle;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AThickness: Integer; AFillStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
  end;


  { TInscribedFigure }

  TInscribedFigure = class(TFilledFigure)
    FigureBounds: TDoubleRect;
    function GetBounds: TDoubleRect; override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle);
  end;

  { TRectangle }

  TRectangle = class(TInscribedFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
    FStartPoint: TDoublePoint;
    FEndPoint: TDoublePoint;
    constructor Create(AMousePos: TDoublePoint; APenColor: TColor;
      ALineStyle: TFPPenStyle; ALineWidth: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TInscribedFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TRectangleLine }

  TRectangleLine = class(TInscribedFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TRegularPolygon }

  TRegularPolygon = class(TFilledFigure)
    FCorners: Integer;
    FVertexes: array of TDoublePoint;
    FCenter: TDoublePoint;
    FCirclePoint: TDoublePoint;
    constructor Create(ACenterPoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle; ACorners: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
  end;

implementation

{ Misc }

function GetTurnAngle(ACenter, AOnCircle: TDoublePoint): Double;
var
  CosAngel: Double;
  VectorX, VectorCircle: TDoublePoint;
  VectorXLength, VectorCircleLength: Double;
begin
  VectorX := DoublePoint(10, 0);
  VectorCircle := AOnCircle - ACenter;
  {TODO: привести к читаемому виду}
  VectorXLength := sqrt(VectorX.X**2 + VectorX.Y**2);
  VectorCircleLength := sqrt(VectorCircle.X**2 + VectorCircle.Y**2);
  CosAngel := (VectorX * VectorCircle)/(VectorXLength * VectorCircleLength);
  if AOnCircle.Y < ACenter.Y then
    Result := 2*Pi - arccos(CosAngel)
  else
    Result := arccos(CosAngel);
end;

function GetVertexesBound(Vertexes: array of TDoublePoint): TDoubleRect;
  var
  i: Integer;
  LeftX, RightX, TopY, BottomY: Double;
begin
  with Vertexes[0] do begin
    LeftX := X;
    RightX := X;
    TopY := Y;
    BottomY := Y;
  end;
  for i := 1 to High(Vertexes) do begin
    with Vertexes[i] do begin
      TopY := Min(TopY, y);
      LeftX := Min(LeftX, X);
      BottomY := Max(BottomY, Y);
      RightX := Max(RightX, X);
    end;
  end;
  Result := DoubleRect(LeftX, TopY, RightX, BottomY);
end;

{ TInscribedFigure }

function TInscribedFigure.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(FigureBounds.Top, FigureBounds.Bottom);
    Left := Min(FigureBounds.Left, FigureBounds.Right);
    Bottom := Max(FigureBounds.Top, FigureBounds.Bottom);
    Right := Max(FigureBounds.Left, FigureBounds.Right);
  end;
end;

procedure TInscribedFigure.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FigureBounds := DoubleRect(FigureBounds.TopLeft, ADoublePoint);
end;

constructor TInscribedFigure.Create(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer;
  AFillStyle: TFPBrushStyle);
begin
  Inherited Create(APenColor, ABrushColor, APenStyle, AThickness, AFillStyle);
  FigureBounds := DoubleRect(ADoublePoint, ADoublePoint);
end;

{ TRegularPolygon }

constructor TRegularPolygon.Create(ACenterPoint: TDoublePoint; APenColor,
  ABrushColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer;
  AFillStyle: TFPBrushStyle; ACorners: Integer);
begin
  Inherited Create(APenColor, ABrushColor, APenStyle, AThickness, AFillStyle);
  FCenter := ACenterPoint;
  FCorners := ACorners;
end;

procedure TRegularPolygon.DrawFigure(Canvas: TCanvas);
var
  i: Integer;
  Radius: Double;
  TurnAngle: Double;
begin
  Radius := sqrt((FCirclePoint.X - FCenter.X)**2 + (FCirclePoint.Y - FCenter.Y)**2);
  { TODO : Улучшить алгоритм }
  SetLength(FVertexes, FCorners);
  TurnAngle := GetTurnAngle(FCenter, FCirclePoint);

  for i := 0 to FCorners - 1 do begin
    FVertexes[i].x := FCenter.X + (Radius*sin((i * 2 * pi / FCorners) - TurnAngle));
    FVertexes[i].y := FCenter.Y + (Radius*cos((i * 2 * pi / FCorners) - TurnAngle));
  end;
  Canvas.Polygon(WorldVertexesToDispCoord(FVertexes));
end;

procedure TRegularPolygon.Draw(Canvas: TCanvas);
begin
  Canvas.Brush.Color := FBrushColor;
  Canvas.Brush.Style := FBrushStyle;
  Inherited;
end;

procedure TRegularPolygon.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FCirclePoint := ADoublePoint;
end;

function TRegularPolygon.GetBounds: TDoubleRect;
begin
  Result := GetVertexesBound(FVertexes);
end;


{ TFilledFigure }

constructor TFilledFigure.Create(APenColor, ABrushColor: TColor;
  APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle);
begin
  Inherited Create(APenColor, APenStyle, AThickness);
  FBrushColor := ABrushColor;
  FBrushStyle := AFillStyle;
end;

procedure TFilledFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Brush.Color := FBrushColor;
  Canvas.Brush.Style := FBrushStyle;
  Inherited;
end;

{ TFigure }
constructor TFigure.Create(APenColor: TColor; APenStyle: TFPPenStyle;
  AThickness: Integer);
begin
  PenColor := APenColor;
  Thickness := AThickness;
  PenStyle := APenStyle;
end;

procedure TFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := PenColor;
  Canvas.Pen.Width := Thickness;
  Canvas.Pen.Style := PenStyle;
  DrawFigure(Canvas);
end;

{ TTwoPointFigure }

constructor TTwoPointFigure.Create(APenColor: TColor; APenStyle: TFPPenStyle;
  AThickness: Integer);
begin
  inherited Create(APenColor, APenStyle, AThickness);
end;

{ TPolyline }
constructor TPolyline.Create(ADoublePoint: TDoublePoint; APenColor: TColor;
  APenStyle: TFPPenStyle; AThickness: Integer);
begin
  inherited Create(APenColor, APenStyle, AThickness);
  AddPoint(ADoublePoint);
end;

procedure TPolyline.AddPoint(ADoublePoint: TDoublePoint);
begin
  SetLength(Vertexes, Length(Vertexes) + 1);
  Vertexes[High(Vertexes)] := ADoublePoint;
end;

procedure TPolyline.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Polyline(WorldVertexesToDispCoord(Vertexes));
end;

function TPolyline.GetBounds: TDoubleRect;
begin
  Result := GetVertexesBound(Vertexes);
end;

{ TRectangle }
procedure TRectangle.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Rectangle(WorldToDispCoord(FigureBounds));
end;

{ TEllipse }
procedure TEllipse.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Ellipse(WorldToDispCoord(FigureBounds));
end;

constructor TLine.Create(AMousePos: TDoublePoint; APenColor: TColor;
  ALineStyle: TFPPenStyle; ALineWidth: Integer);
begin
  Inherited Create(APenColor, ALineStyle, ALineWidth);
  FStartPoint := AMousePos;
end;

procedure TLine.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FEndPoint := ADoublePoint;
end;

function TLine.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(FStartPoint.Y, FEndPoint.Y);
    Left := Min(FStartPoint.X, FEndPoint.X);
    Bottom := Max(FStartPoint.Y, FEndPoint.Y);
    Right := Max(FStartPoint.X, FEndPoint.X);
  end;
end;

{ TLine }
procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(WorldToDispCoord(FStartPoint), WorldToDispCoord(FEndPoint));
end;

{ TRectangleLine }
procedure TRectangleLine.DrawFigure(Canvas: TCanvas);
var
  DispBounds: TRect;
begin
  DispBounds := WorldToDispCoord(FigureBounds);
  with Canvas do begin
    MoveTo(DispBounds.Left, DispBounds.Top);
    LineTo(DispBounds.Right, DispBounds.Top);
    LineTo(DispBounds.Right, DispBounds.Bottom);
    LineTo(DispBounds.Left, DispBounds.Bottom);
    LineTo(DispBounds.Left, DispBounds.Top);
  end;
end;

end.

