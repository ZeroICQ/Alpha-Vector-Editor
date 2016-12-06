unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  UTransform, GraphMath, windows, Classes, SysUtils, Graphics, FPCanvas, LCL, math;

type

  { TFigure }

  TFigure = class
    FIsSelected: Boolean;
    FPenColor: TColor;
    FPenStyle: TFPPenStyle;
    FThickness: Integer;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure DrawFigure(ACanvas: TCanvas); virtual; abstract;
    function GetBounds: TDoubleRect; virtual; abstract;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; virtual; abstract;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; virtual; abstract;
  end;

  { TPolyline }

  TPolyline = class(TFigure)
    FVertexes: array of TDoublePoint;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer);
    procedure AddPoint(ADoublePoint: TDoublePoint);
    procedure DrawFigure(ACanvas: TCanvas); override;
    function GetBounds: TDoubleRect; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
  end;

  { TTwoPointFigure }

  TTwoPointFigure = class(TFigure)
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); virtual; abstract;
    constructor Create(
      APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
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
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TRoundRectangle }

  TRoundRectangle = class(TInscribedFigure)
    FFactorX: Integer;
    FFactorY: Integer;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle;
      AFactorX, AFactorY: Integer);
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
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
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TEllipse }

  TEllipse = class(TInscribedFigure)
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
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
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

var
  Figures: array of TFigure;
implementation

{ Misc }

//взято из winapi
function RoundRectPolygon(X1, Y1, X2, Y2: Integer; RX,RY : Integer): HRGN;
var
  T: Integer;
  Points: PPoint;
  Count: Integer;

  procedure AddArcPoints(Left, Top, Right, Bottom, Angle1, Angle2: Integer);
  var
    P: PPoint;
    C: Integer;
    I: Integer;
  begin
    P := nil;
    try
      PolyBezierArcPoints(Left, Top, Right - Left, Bottom - Top, Angle1, Angle2,
        0, P, C);
      ReallocMem(Points, (Count + C) * SizeOf(TPoint));
      for I := 0 to Pred(C) do
        Points[Count + Pred(C) - I] := P[I];
      Inc(Count, C);
    finally
      FreeMem(P);
    end;
  end;
begin
  if X2 < X1 then
  begin
    T := X1;
    X1 := X2;
    X2 := T;
  end;
  if Y2 < Y1 then
  begin
    T := Y1;
    Y1 := Y2;
    Y2 := T;
  end;
  if (X2 - X1 <= 0) or (Y2 - Y1 <= 0) then Exit;
  Dec(X2);
  Dec(Y2);
  if not ((RX <= 0) or (RY <= 0)) then
  begin
    if X2 - X1 < RX then RX := X2 - X1;
    if Y2 - Y1 < RY then RY := Y2 - Y1;
    Points := nil;
    Count := 0;
    try
      AddArcPoints(X1, Y1, X1 + RX, Y1 + RY,  90 * 16, 90 * 16);
      AddArcPoints(X2 - RX, Y1, X2, Y1 + RY,   0 * 16, 90 * 16);
      AddArcPoints(X2 - RX, Y2 - RY, X2, Y2, 270 * 16, 90 * 16);
      AddArcPoints(X1, Y2 - RY, X1 + RX, Y2, 180 * 16, 90 * 16);
      Result := CreatePolygonRgn(Points[0], Count, WINDING);
    finally
      FreeMem(Points);
    end;
  end
  else
    Result := CreateRectRgn(X1, Y1, X2, Y2);
end;

function IntersectSegments(APointA, APointB, APointC, APointD: TDoublePoint): Boolean;
var
  v1,v2,v3,v4: Double;
  ax1, ay1, ax2, ay2, bx1, by1, bx2, by2: Double;
begin
  ax1 := APointA.X; ay1 := APointA.Y;
  ax2 := APointB.X; ay2 := APointB.Y;
  bx1 := APointC.X; by1 := APointC.Y;
  bx2 := APointD.X; by2 := APointD.Y;

  v1 := (bx2-bx1) * (ay1-by1) - (by2-by1) * (ax1-bx1);
  v2 := (bx2-bx1) * (ay2-by1) - (by2-by1) * (ax2-bx1);
  v3 := (ax2-ax1) * (by1-ay1) - (ay2-ay1) * (bx1-ax1);
  v4 := (ax2-ax1) * (by2-ay1) - (ay2-ay1) * (bx2-ax1);
  Result := (v1 * v2 < 0) and (v3 * v4 < 0);
end;

function IntersectRect(AStartPoint, AEndPoint: TDoublePoint; ADoubleRect: TDoubleRect): Boolean;
begin
  with ADoubleRect do begin
    Result :=
      IntersectSegments(AStartPoint, AEndPoint, DoublePoint(Left, Bottom), TopLeft) or
      IntersectSegments(AStartPoint, AEndPoint, TopLeft, DoublePoint(Right, Top)) or
      IntersectSegments(AStartPoint, AEndPoint, DoublePoint(Right, Top), BottomRight) or
      IntersectSegments(AStartPoint, AEndPoint, BottomRight, DoublePoint(Left, Bottom));
  end;
end;

function PointInsideSegment(AStartPoint, AEndPoint: TDoublePoint;
  ADoublePoint: TDoublePoint; AThickness: Integer): Boolean;
var
  VecA: TPoint;
  VecB: TPoint;
  VecC: TPoint;
  Point: TPoint;
  StartPoint, EndPoint: TPoint;
begin
  Point := WorldToDispCoord(ADoublePoint);
  StartPoint := WorldToDispCoord(AStartPoint);
  EndPoint :=  WorldToDispCoord(AEndPoint);
  VecA := EndPoint - StartPoint;
  VecB := Point - StartPoint;
  VecC := Point - EndPoint;
  AThickness := AThickness div 2;
  {Вынести в функцию}
  Result :=
    ((sqrt(VecC.x**2 + VecC.y**2) + sqrt(VecB.x**2 + VecB.y**2)) <= sqrt(VecA.x**2 + VecA.y**2)+1);
end;

function PointInsideRect(ADoublePoint: TDoublePoint; ADoubleRect: TDoubleRect): Boolean;
begin
  with ADoublePoint, ADoubleRect do begin
    Result := (X <= Right) and (X >= Left) and (Y >= Top) and (Y <= Bottom);
  end;
end;

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

function TRoundRectangle.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
begin
  with WorldToDispCoord(FFigureBounds) do
    Result := RectInRegion(
      RoundRectPolygon(Left, Top, Right, Bottom, FFactorX, FFactorY),
      WorldToDispCoord(ADoubleRect));
end;

function TRoundRectangle.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var Point: TPoint;
begin
  Point := WorldToDispCoord(ADoublePoint);
  with WorldToDispCoord(FFigureBounds) do
    Result := PtInRegion(
      RoundRectPolygon(Left, Top, Right, Bottom, FFactorX, FFactorY),
      Point.x, Point.y);
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
  FCirclePoint.X := ACenterPoint.X + 1;
  FCirclePoint.Y := ACenterPoint.Y + 1;
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

procedure TRegularPolygon.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FCirclePoint := ADoublePoint;
end;

function TRegularPolygon.GetBounds: TDoubleRect;
begin
  Result := GetVertexesBound(FVertexes);
end;

function TRegularPolygon.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
begin
  SetLength(Points, FCorners);
  Points := WorldVertexesToDispCoord(FVertexes);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Result := RectInRegion(Polygon, WorldToDispCoord(ADoubleRect));
  DeleteObject(Polygon);
end;

function TRegularPolygon.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
  Point: TPoint;
begin
  SetLength(Points, FCorners);
  Points := WorldVertexesToDispCoord(FVertexes);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(Polygon, Point.x, Point.y);
  DeleteObject(Polygon);
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
  if FIsSelected then begin
    with GetBounds do begin;
      ACanvas.Pen.Style := psDash;
      ACanvas.Pen.Color := clBlue;
      ACanvas.Pen.Width := 2;
      ACanvas.Frame(WorldToDispCoord(DoubleRect(
        TopLeft - FThickness - 5 / Scale, BottomRight + FThickness + 5 / Scale)));
    end;
  end;
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

function TPolyline.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var i: Integer;
begin
  for i := Low(FVertexes) to High(FVertexes) - 1 do begin
    if PointInsideSegment(FVertexes[i], FVertexes[i+1], ADoublePoint, FThickness) then
      Exit(True);
  end;
  Result := False;
end;

function TPolyline.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  i: Integer;
begin
  for i := Low(FVertexes) to High(FVertexes) do begin
    if PointInsideRect(FVertexes[i], ADoubleRect) then Exit(True);
    if i < High(FVertexes) - 1 then begin
      if IntersectRect(FVertexes[i], FVertexes[i+1], ADoubleRect) then Exit(True);
    end;
  end;
  Result := False;
end;

{ TRectangle }
procedure TRectangle.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Rectangle(WorldToDispCoord(FFigureBounds));
end;

function TRectangle.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  Rect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    Rect := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  Result := RectInRegion(Rect, WorldToDispCoord(ADoubleRect));
  DeleteObject(Rect);
end;

function TRectangle.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Point: TPoint;
  Rect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    Rect := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(Rect, Point.X, Point.Y);
  DeleteObject(Rect);
end;

{ TEllipse }
procedure TEllipse.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Ellipse(WorldToDispCoord(FFigureBounds));
end;

function TEllipse.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  Ellipse: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    Ellipse := CreateEllipticRgn(Left, Top, Right, Bottom);
  end;
  Result := RectInRegion(Ellipse, WorldToDispCoord(ADoubleRect));
  DeleteObject(Ellipse);
end;

function TEllipse.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Point: TPoint;
  Ellipse: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    Ellipse := CreateEllipticRgn(Left, Top, Right, Bottom);
  end;
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(Ellipse, Point.X, Point.Y);
  DeleteObject(Ellipse);
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

function TLine.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
begin
  with ADoubleRect do begin
    Result :=
      IntersectRect(FStartPoint, FEndPoint, ADoubleRect) or
      PointInsideRect(FStartPoint, ADoubleRect) or
      PointInsideRect(FEndPoint, ADoubleRect);
  end;
end;

function TLine.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
begin
  Result := PointInsideSegment(FStartPoint, FEndPoint, ADoublePoint, FThickness);
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

