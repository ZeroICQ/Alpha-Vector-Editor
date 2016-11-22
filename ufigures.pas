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
    FigureBounds: TDoubleRect;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint);
    function GetBounds: TDoubleRect; override;
  end;

  { TFilledFigure }

  TFilledFigure = class(TTwoPointFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TFilledFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TFilledFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TRectangleLine }

  TRectangleLine = class(TTwoPointFigure)
    procedure DrawFigure(Canvas: TCanvas); override;
  end;

  { TRegularPolygon }

  TRegularPolygon = class(TFilledFigure)
    FCorners: Integer;
    Vertexes: array of TDoublePoint;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle; ACorners: Integer);
    procedure DrawFigure(Canvas: TCanvas); override;
    function GetBounds: TDoubleRect; override;
  end;

implementation

{ Misc }

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

{ TRegularPolygon }

constructor TRegularPolygon.Create(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer;
  AFillStyle: TFPBrushStyle; ACorners: Integer);
begin
  Inherited Create(ADoublePoint, APenColor, ABrushColor, APenStyle, AThickness, AFillStyle);
  FCorners := ACorners;
end;

procedure TRegularPolygon.DrawFigure(Canvas: TCanvas);
var
  i: Integer;
  WrldFigureCenter: TDoublePoint;
  Radius: Double;
begin
  WrldFigureCenter := FigureBounds.TopLeft +
    (FigureBounds.BottomRight - FigureBounds.TopLeft) / 2;
  Radius := Min(
    FigureBounds.Right - WrldFigureCenter.x, FigureBounds.Bottom - WrldFigureCenter.Y);
  { TODO : Улучшить алгоритм }
  SetLength(Vertexes, FCorners);
  for i := 0 to FCorners - 1 do begin
    Vertexes[i].x := WrldFigureCenter.X + (Radius*sin(i * 2 * pi / FCorners));
    Vertexes[i].y := WrldFigureCenter.Y + (Radius*cos(i * 2 * pi / FCorners));
  end;
  Canvas.Polygon(WorldVertexesToDispCoord(Vertexes));
end;

function TRegularPolygon.GetBounds: TDoubleRect;
begin
  Result := GetVertexesBound(Vertexes);
end;


{ TFilledFigure }

constructor TFilledFigure.Create(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer;
  AFillStyle: TFPBrushStyle);
begin
  Inherited Create(ADoublePoint, APenColor, APenStyle, AThickness);
  BrushColor := ABrushColor;
  BrushStyle := AFillStyle;
end;

procedure TFilledFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Brush.Color := BrushColor;
  Canvas.Brush.Style := BrushStyle;
  inherited Draw(canvas);
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

constructor TTwoPointFigure.Create(ADoublePoint: TDoublePoint;
  APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
begin
  inherited Create(APenColor, APenStyle, AThickness);
  FigureBounds := DoubleRect(ADoublePoint, ADoublePoint);
end;

procedure TTwoPointFigure.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FigureBounds := DoubleRect(FigureBounds.TopLeft, ADoublePoint);
end;

function TTwoPointFigure.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(FigureBounds.Top, FigureBounds.Bottom);
    Left := Min(FigureBounds.Left, FigureBounds.Right);
    Bottom := Max(FigureBounds.Top, FigureBounds.Bottom);
    Right := Max(FigureBounds.Left, FigureBounds.Right);
  end;
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

{ TLine }
procedure TLine.DrawFigure(Canvas: TCanvas);
begin
  Canvas.Line(WorldToDispCoord(FigureBounds));
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

