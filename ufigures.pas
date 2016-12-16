unit UFigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Controls, UTransform, windows, Classes, SysUtils, Graphics, FPCanvas, LCL,
  math, UParamEditor, UParameters, UAppState;

type
  { TFigure }

  TFigure = class(TPersistent)
  private
    FIsSelected: Boolean;
    FParams: array of TParam;
    FPenColor: TColor;
    FLineWidth: TParamLineWidth;
    FLineStyle: TParamLineStyle;
  published
    property FParamLineWidth: TParamLineWidth read FLineWidth write FLineWidth;
    property FParamLineStyle: TParamLineStyle read FLineStyle write FLineStyle;
    property FParamPenColor: TColor read FPenColor write FPenColor;
  public
    property IsSelected: Boolean read FIsSelected write FIsSelected;
    procedure DrawFigure(ACanvas: TCanvas); virtual; abstract;
    procedure AddParam(AParam: TParam);
    function GetParams: TParamArr;
    procedure Move(ADisplacement: TDoublePoint); virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual;
    function GetBounds: TDoubleRect; virtual; abstract;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; virtual; abstract;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; virtual; abstract;
    constructor Create(APenColor: TColor);
  end;

  { TPolyline }

  TPolyline = class(TFigure)
  private
    FVertexes: array of TDoublePoint;
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Move(ADisplacement: TDoublePoint); override;
    procedure AddPoint(ADoublePoint: TDoublePoint);
    function GetBounds: TDoubleRect; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    constructor Create(AFirstDoublePoint: TDoublePoint; APenColor: TColor);
  end;

  { TTwoPointFigure }

  TTwoPointFigure = class(TFigure)
  public
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); virtual; abstract;
  end;

  { TFilledFigure }

  TFilledFigure = class(TTwoPointFigure)
  private
    FBrushColor: TColor;
  published
    FParamBrushStyle: TParamBrushStyle;
    property FParamBrushColor: TColor read FBrushColor write FBrushColor;
  public
    procedure Draw(ACanvas: TCanvas); override;
    constructor Create(APenColor, ABrushColor: TColor);
  end;

  { TInscribedFigure }

  TInscribedFigure = class(TFilledFigure)
  private
    FFigureBounds: TDoubleRect;
  published

  public
    property FParamFigureBounds: TDoubleRect read FFigureBounds write FFigureBounds;
    procedure Move(ADisplacement: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor);
  end;

  { TRectangle }

  TRectangle = class(TInscribedFigure)
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TRoundRectangle }

  TRoundRectangle = class(TInscribedFigure)
  published
    FParamXCoeff: TParamXCoeff;
    FParamYCoef: TParamYCoeff;
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor);
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
  public
    FParamStartPoint: TDoublePoint;
    FParamEndPoint: TDoublePoint;
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Move(ADisplacement: TDoublePoint); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TEllipse }

  TEllipse = class(TInscribedFigure)
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TSelection }

  TSelection = class(TInscribedFigure)
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    constructor Create(ADoublePoint: TDoublePoint);
  end;

  { TRegularPolygon }

  TRegularPolygon = class(TFilledFigure)
  public
    FParamVertexes: array of TDoublePoint;
    FParamCenter: TDoublePoint;
    FParamCirclePoint: TDoublePoint;
    FParamCorners: TParamCorners;
  public
    procedure UpdateFigure;
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Move(ADisplacement: TDoublePoint); override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    constructor Create(ACenterPoint: TDoublePoint; APenColor, ABrushColor: TColor);
  end;

var
  Figures: array of TFigure;

implementation

{ Misc }

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
  A, B, C: Double;
  Thickness: Double;
begin
  Thickness := (AThickness / 2 / Scale);
  A := AStartPoint.Y - AEndPoint.Y;
  B := AEndPoint.X - AStartPoint.X;
  C := (AStartPoint.X * AEndPoint.Y) - (AEndPoint.X * AStartPoint.Y);
  with ADoublePoint do
    Result :=
      (abs(A*X + B*Y + C) / sqrt(A**2 + B**2) <= Thickness) and
      (((AStartPoint.X - Thickness < X) and (X < AEndPoint.X + Thickness)) or
          (AEndPoint.X - Thickness < X) and (X < AStartPoint.X + Thickness)) and
      (((AStartPoint.Y - Thickness < Y) and (Y < AEndPoint.Y + Thickness)) or
          (AEndPoint.Y - Thickness < Y) and (Y < AStartPoint.Y + Thickness));
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
  ABrushColor: TColor);
begin
  Inherited Create(ADoublePoint, APenColor, ABrushColor);
  FParamXCoeff := TParamXCoeff.Create;
  AddParam(FParamXCoeff);
  FParamYCoef := TParamYCoeff.Create;
  AddParam(FParamYCoef);
end;

procedure TRoundRectangle.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.RoundRect(WorldToDispCoord(FParamFigureBounds), FParamXCoeff.Value, FParamYCoef.Value);
end;

function TRoundRectangle.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  RoundRect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FParamXCoeff.Value, FParamYCoef.Value);
  end;
  Result := RectInRegion(RoundRect, WorldToDispCoord(ADoubleRect));
  DeleteObject(RoundRect);
end;

function TRoundRectangle.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Point: TPoint;
  RoundRect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FParamXCoeff.Value, FParamYCoef.Value);
  end;
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(RoundRect, Point.X, Point.Y);
  DeleteObject(RoundRect);
end;


{ TInscribedFigure }

procedure TInscribedFigure.Move(ADisplacement: TDoublePoint);
begin
  FFigureBounds.BottomRight += ADisplacement;
  FFigureBounds.TopLeft += ADisplacement;
end;

function TInscribedFigure.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(FParamFigureBounds.Top, FParamFigureBounds.Bottom);
    Left := Min(FParamFigureBounds.Left, FParamFigureBounds.Right);
    Bottom := Max(FParamFigureBounds.Top, FParamFigureBounds.Bottom);
    Right := Max(FParamFigureBounds.Left, FParamFigureBounds.Right);
  end;
end;

procedure TInscribedFigure.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FParamFigureBounds := DoubleRect(FParamFigureBounds.TopLeft, ADoublePoint);
end;

constructor TInscribedFigure.Create(ADoublePoint: TDoublePoint; APenColor,
  ABrushColor: TColor);
begin
  Inherited Create(APenColor, ABrushColor);
  FParamFigureBounds := DoubleRect(ADoublePoint, ADoublePoint);
end;

{ TRegularPolygon }

constructor TRegularPolygon.Create(ACenterPoint: TDoublePoint; APenColor,
  ABrushColor: TColor);
begin
  Inherited Create(APenColor, ABrushColor);
  FParamCenter := ACenterPoint;
  FParamCirclePoint := ACenterPoint + 1;

  FParamCorners := TParamCorners.Create;
  AddParam(FParamCorners);
end;

procedure TRegularPolygon.DrawFigure(ACanvas: TCanvas);
begin
  UpdateFigure;
  ACanvas.Polygon(WorldVertexesToDispCoord(FParamVertexes));
end;

procedure TRegularPolygon.UpdateFigure;
var
  i: Integer;
  Radius: Double;
  TurnAngle: Double;
begin
  Radius := sqrt((FParamCirclePoint.X - FParamCenter.X)**2 + (FParamCirclePoint.Y - FParamCenter.Y)**2);
  { TODO : Улучшить алгоритм }
  SetLength(FParamVertexes, FParamCorners.Corners);
  TurnAngle := GetTurnAngle(FParamCenter, FParamCirclePoint);
  for i := 0 to FParamCorners.Corners - 1 do begin
    FParamVertexes[i].x := FParamCenter.X + (Radius*sin((i * 2 * pi / FParamCorners.Corners) - TurnAngle));
    FParamVertexes[i].y := FParamCenter.Y + (Radius*cos((i * 2 * pi / FParamCorners.Corners) - TurnAngle));
  end;
end;

procedure TRegularPolygon.Move(ADisplacement: TDoublePoint);
begin
  FParamCenter += ADisplacement;
  FParamCirclePoint += ADisplacement;
end;

procedure TRegularPolygon.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FParamCirclePoint := ADoublePoint;
end;

function TRegularPolygon.GetBounds: TDoubleRect;
begin
  UpdateFigure;
  Result := GetVertexesBound(FParamVertexes);
end;

function TRegularPolygon.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
begin
  SetLength(Points, FParamCorners.Corners);
  Points := WorldVertexesToDispCoord(FParamVertexes);
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
  SetLength(Points, FParamCorners.Corners);
  Points := WorldVertexesToDispCoord(FParamVertexes);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(Polygon, Point.x, Point.y);
  DeleteObject(Polygon);
end;

{ TFilledFigure }

constructor TFilledFigure.Create(APenColor, ABrushColor: TColor);
begin
  Inherited Create(APenColor);
  FParamBrushColor := ABrushColor;

  FParamBrushStyle := TParamBrushStyle.Create;
  AddParam(FParamBrushStyle);
end;

procedure TFilledFigure.Draw(ACanvas: TCanvas);
begin
  with ACanvas do begin
    Brush.Color := FParamBrushColor;
    Brush.Style := FParamBrushStyle.Style;
  end;
  Inherited;
end;

procedure TFigure.AddParam(AParam: TParam);
begin
  SetLength(FParams, Length(FParams) + 1);
  FParams[High(FParams)] := AParam;
end;

function TFigure.GetParams: TParamArr;
begin
  Result := FParams;
end;

{ TFigure }
constructor TFigure.Create(APenColor: TColor);
begin
  SetAppStateModified;
  IsSelected := False;
  FPenColor := APenColor;
  FParamLineWidth := TParamLineWidth.Create;
  AddParam(FParamLineWidth);
  FParamLineStyle := TParamLineStyle.Create;
  AddParam(FParamLineStyle);
end;

procedure TFigure.Draw(ACanvas: TCanvas);
var
  FrameCoords: TRect;
  i: Integer;
begin
  with ACanvas, GetBounds do begin
    if IsSelected then begin
      Pen.Style := psDash;
      Pen.Color := clRed;
      Pen.Width := 3;
      FrameCoords := WorldToDispCoord(DoubleRect(TopLeft, BottomRight));
      FrameCoords.TopLeft -= FParamLineWidth.Width div 2 + 5;
      FrameCoords.BottomRight  += FParamLineWidth.Width div 2 + 5;
      Frame(FrameCoords);
    end;
    Pen.Color := FPenColor;
    for i := Low(FParams) to High(FParams) do FParams[i].Apply(ACanvas);
  end;
  DrawFigure(ACanvas);
end;

{ TPolyline }
constructor TPolyline.Create(AFirstDoublePoint: TDoublePoint; APenColor: TColor);
begin
  Inherited Create(APenColor);
  AddPoint(AFirstDoublePoint);
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

procedure TPolyline.Move(ADisplacement: TDoublePoint);
var i: Integer;
begin
  for i := Low(FVertexes) to High(FVertexes) do begin
    FVertexes[i] += ADisplacement;
  end;
end;

function TPolyline.GetBounds: TDoubleRect;
begin
  Result := GetVertexesBound(FVertexes);
end;

function TPolyline.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var i: Integer;
begin
  for i := Low(FVertexes) to High(FVertexes) - 1 do begin
    if PointInsideSegment(FVertexes[i], FVertexes[i+1], ADoublePoint, FParamLineWidth.Width) then
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
  ACanvas.Rectangle(WorldToDispCoord(FParamFigureBounds));
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
  ACanvas.Ellipse(WorldToDispCoord(FParamFigureBounds));
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

constructor TLine.Create(ADoublePoint: TDoublePoint; APenColor: TColor);
begin
  Inherited Create(APenColor);
  FParamStartPoint := ADoublePoint;
  FParamEndPoint := ADoublePoint;
end;

procedure TLine.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FParamEndPoint := ADoublePoint;
end;

function TLine.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(FParamStartPoint.Y, FParamEndPoint.Y);
    Left := Min(FParamStartPoint.X, FParamEndPoint.X);
    Bottom := Max(FParamStartPoint.Y, FParamEndPoint.Y);
    Right := Max(FParamStartPoint.X, FParamEndPoint.X);
  end;
end;

procedure TLine.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Line(WorldToDispCoord(FParamStartPoint), WorldToDispCoord(FParamEndPoint));
end;

procedure TLine.Move(ADisplacement: TDoublePoint);
begin
  FParamStartPoint += ADisplacement;
  FParamEndPoint += ADisplacement;
end;

function TLine.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
begin
  with ADoubleRect do begin
    Result :=
      IntersectRect(FParamStartPoint, FParamEndPoint, ADoubleRect) or
      PointInsideRect(FParamStartPoint, ADoubleRect) or
      PointInsideRect(FParamEndPoint, ADoubleRect);
  end;
end;

function TLine.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
begin
  Result := PointInsideSegment(FParamStartPoint, FParamEndPoint, ADoublePoint, FParamLineWidth.Width);
end;

{ TSelection }
procedure TSelection.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Frame(WorldToDispCoord(FParamFigureBounds));
end;

constructor TSelection.Create(ADoublePoint: TDoublePoint);
begin
  Inherited Create(ADoublePoint, clBlack, clWhite);
  FParamLineWidth.SetValue(1);
  FParamLineStyle.SetValue(psDash);
end;

end.

