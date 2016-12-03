unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  UTransform, Classes, SysUtils, Graphics, math, FPCanvas;

type

  { TFigure }

  TFigure = class
    FPenColor: TColor;
    FPenStyle: TFPPenStyle;
    FThickness: Integer;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure DrawFigure(ACanvas: TCanvas); virtual; abstract;
    function GetBounds: TDoubleRect; virtual; abstract;
  end;

  { TPolyline }

  TPolyline = class(TFigure)
    FVertexes: array of TDoublePoint;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer);
    procedure AddPoint(ADoublePoint: TDoublePoint);
    procedure DrawFigure(ACanvas: TCanvas); override;
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
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TInscribedFigure }

  TInscribedFigure = class(TFilledFigure)
    FFigureBounds: TDoubleRect;
    function GetBounds: TDoubleRect; override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle);
  end;

  { TRectangle }

  TRectangle = class(TInscribedFigure)
    procedure DrawFigure(ACanvas: TCanvas); override;
  end;

  { TRoundRectangle }

  TRoundRectangle = class(TInscribedFigure)
    FFactorX: Integer;
    FFactorY: Integer;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle;
      AFactorX, AFactorY: Integer);
    procedure DrawFigure(ACanvas: TCanvas); override;
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
    FStartPoint: TDoublePoint;
    FEndPoint: TDoublePoint;
    constructor Create(AMousePos: TDoublePoint; APenColor: TColor;
      ALineStyle: TFPPenStyle; ALineWidth: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    procedure DrawFigure(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TInscribedFigure)
    procedure DrawFigure(ACanvas: TCanvas); override;
  end;

  { TSelection }

  TSelection = class(TInscribedFigure)
    constructor Create(ADoublePoint: TDoublePoint);
    procedure DrawFigure(ACanvas: TCanvas); override;
  end;

  { TRegularPolygon }

  TRegularPolygon = class(TFilledFigure)
    FCorners: Integer;
    FVertexes: array of TDoublePoint;
    FCenter: TDoublePoint;
    FCirclePoint: TDoublePoint;
    constructor Create(ACenterPoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle; ACorners: Integer);
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Draw(ACanvas: TCanvas); override;
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

function GetVertexesBound(AVertexes: array of TDoublePoint): TDoubleRect;
  var
  i: Integer;
  LeftX, RightX, TopY, BottomY: Double;
begin
  with AVertexes[0] do begin
    LeftX := X;
    RightX := X;
    TopY := Y;
    BottomY := Y;
  end;
  for i := 1 to High(AVertexes) do begin
    with AVertexes[i] do begin
      TopY := Min(TopY, y);
      LeftX := Min(LeftX, X);
      BottomY := Max(BottomY, Y);
      RightX := Max(RightX, X);
    end;
  end;
  Result := DoubleRect(LeftX, TopY, RightX, BottomY);
end;

{ TRoundRectangle }

constructor TRoundRectangle.Create(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer;
  AFillStyle: TFPBrushStyle; AFactorX, AFactorY: Integer);
begin
  Inherited Create(ADoublePoint, APenColor, ABrushColor, APenStyle, AThickness, AFillStyle);
  FFactorX := AFactorX;
  FFactorY := AFactorY;
end;

procedure TRoundRectangle.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.RoundRect(WorldToDispCoord(FFigureBounds), FFactorX, FFactorY);
end;

{ TInscribedFigure }

function TInscribedFigure.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(FFigureBounds.Top, FFigureBounds.Bottom);
    Left := Min(FFigureBounds.Left, FFigureBounds.Right);
    Bottom := Max(FFigureBounds.Top, FFigureBounds.Bottom);
    Right := Max(FFigureBounds.Left, FFigureBounds.Right);
  end;
end;

procedure TInscribedFigure.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FFigureBounds := DoubleRect(FFigureBounds.TopLeft, ADoublePoint);
end;

constructor TInscribedFigure.Create(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer;
  AFillStyle: TFPBrushStyle);
begin
  Inherited Create(APenColor, ABrushColor, APenStyle, AThickness, AFillStyle);
  FFigureBounds := DoubleRect(ADoublePoint, ADoublePoint);
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

procedure TRegularPolygon.DrawFigure(ACanvas: TCanvas);
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
  ACanvas.Polygon(WorldVertexesToDispCoord(FVertexes));
end;

procedure TRegularPolygon.Draw(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
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

procedure TFilledFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
  Inherited;
end;

{ TFigure }
constructor TFigure.Create(APenColor: TColor; APenStyle: TFPPenStyle;
  AThickness: Integer);
begin
  FPenColor := APenColor;
  FThickness := AThickness;
  FPenStyle := APenStyle;
end;

procedure TFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Width := FThickness;
  ACanvas.Pen.Style := FPenStyle;
  DrawFigure(ACanvas);
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
  SetLength(FVertexes, Length(FVertexes) + 1);
  FVertexes[High(FVertexes)] := ADoublePoint;
end;

procedure TPolyline.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Polyline(WorldVertexesToDispCoord(FVertexes));
end;

function TPolyline.GetBounds: TDoubleRect;
begin
  Result := GetVertexesBound(FVertexes);
end;

{ TRectangle }
procedure TRectangle.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Rectangle(WorldToDispCoord(FFigureBounds));
end;

{ TEllipse }
procedure TEllipse.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Ellipse(WorldToDispCoord(FFigureBounds));
end;

{ TLine }

constructor TLine.Create(AMousePos: TDoublePoint; APenColor: TColor;
  ALineStyle: TFPPenStyle; ALineWidth: Integer);
begin
  Inherited Create(APenColor, ALineStyle, ALineWidth);
  FStartPoint := AMousePos;
  FEndPoint := AMousePos;
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

procedure TLine.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Line(WorldToDispCoord(FStartPoint), WorldToDispCoord(FEndPoint));
end;

{ TSelection }
procedure TSelection.DrawFigure(ACanvas: TCanvas);
var
  DispBounds: TRect;
begin
  DispBounds := WorldToDispCoord(FFigureBounds);
  ACanvas.Frame(DispBounds);
end;

constructor TSelection.Create(ADoublePoint: TDoublePoint);
begin
  Inherited Create(ADoublePoint, clBlack, clBlack, psDash, 2, bsSolid);
end;

end.

