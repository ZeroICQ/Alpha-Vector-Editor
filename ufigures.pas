unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Controls, UTransform, windows, Classes, SysUtils, Graphics, FPCanvas, LCL,
  math, UParamEditor, UParameters;

type
  { TFigure }

  TFigure = class
  private
    FIsSelected: Boolean;
    FPenColor: TColor;
    FLineWidth: TParamLineWidth;
    FLineStyle: TParamLineStyle;
    FParams: array of TParam;
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
    FBrushStyle: TParamBrushStyle;
  public
    procedure Draw(ACanvas: TCanvas); override;
    constructor Create(APenColor, ABrushColor: TColor);
  end;

  { TInscribedFigure }

  TInscribedFigure = class(TFilledFigure)
  private
    FFigureBounds: TDoubleRect;
  public
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
  private
    FXCoef: TParamXCoeff;
    FYCoef: TParamYCoeff;
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor);
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
  private
    FStartPoint: TDoublePoint;
    FEndPoint: TDoublePoint;
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
  private
    FVertexes: array of TDoublePoint;
    FCenter: TDoublePoint;
    FCirclePoint: TDoublePoint;
    FCorners: TParamCorners;
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
  ABrushColor: TColor);
begin
  Inherited Create(ADoublePoint, APenColor, ABrushColor);
  FXCoef := TParamXCoeff.Create;
  AddParam(FXCoef);
  FYCoef := TParamYCoeff.Create;
  AddParam(FYCoef);
end;

procedure TRoundRectangle.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.RoundRect(WorldToDispCoord(FFigureBounds), FXCoef.Value, FYCoef.Value);
end;

function TRoundRectangle.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  RoundRect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FXCoef.Value, FYCoef.Value);
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
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FXCoef.Value, FYCoef.Value);
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
  ABrushColor: TColor);
begin
  Inherited Create(APenColor, ABrushColor);
  FFigureBounds := DoubleRect(ADoublePoint, ADoublePoint);
end;

{ TRegularPolygon }

constructor TRegularPolygon.Create(ACenterPoint: TDoublePoint; APenColor,
  ABrushColor: TColor);
begin
  Inherited Create(APenColor, ABrushColor);
  FCenter := ACenterPoint;
  FCirclePoint := ACenterPoint + 1;

  FCorners := TParamCorners.Create;
  AddParam(FCorners);
end;

procedure TRegularPolygon.DrawFigure(ACanvas: TCanvas);
begin
  UpdateFigure;
  ACanvas.Polygon(WorldVertexesToDispCoord(FVertexes));
end;

procedure TRegularPolygon.UpdateFigure;
var
  i: Integer;
  Radius: Double;
  TurnAngle: Double;
begin
  Radius := sqrt((FCirclePoint.X - FCenter.X)**2 + (FCirclePoint.Y - FCenter.Y)**2);
  { TODO : Улучшить алгоритм }
  SetLength(FVertexes, FCorners.Corners);
  TurnAngle := GetTurnAngle(FCenter, FCirclePoint);
  for i := 0 to FCorners.Corners - 1 do begin
    FVertexes[i].x := FCenter.X + (Radius*sin((i * 2 * pi / FCorners.Corners) - TurnAngle));
    FVertexes[i].y := FCenter.Y + (Radius*cos((i * 2 * pi / FCorners.Corners) - TurnAngle));
  end;
end;

procedure TRegularPolygon.Move(ADisplacement: TDoublePoint);
begin
  FCenter += ADisplacement;
  FCirclePoint += ADisplacement;
end;

procedure TRegularPolygon.SetSecondPoint(ADoublePoint: TDoublePoint);
begin
  FCirclePoint := ADoublePoint;
end;

function TRegularPolygon.GetBounds: TDoubleRect;
begin
  UpdateFigure;
  Result := GetVertexesBound(FVertexes);
end;

function TRegularPolygon.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
begin
  SetLength(Points, FCorners.Corners);
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
  SetLength(Points, FCorners.Corners);
  Points := WorldVertexesToDispCoord(FVertexes);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(Polygon, Point.x, Point.y);
  DeleteObject(Polygon);
end;

{ TFilledFigure }

constructor TFilledFigure.Create(APenColor, ABrushColor: TColor);
begin
  Inherited Create(APenColor);
  FBrushColor := ABrushColor;

  FBrushStyle := TParamBrushStyle.Create;
  AddParam(FBrushStyle);
end;

procedure TFilledFigure.Draw(ACanvas: TCanvas);
begin
  with ACanvas do begin
    Brush.Color := FBrushColor;
    Brush.Style := FBrushStyle.Style;
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
  IsSelected := False;
  FPenColor := APenColor;
  FLineWidth := TParamLineWidth.Create;
  AddParam(FLineWidth);
  FLineStyle := TParamLineStyle.Create;
  AddParam(FLineStyle);
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
      FrameCoords.TopLeft -= FLineWidth.Width div 2 + 5;
      FrameCoords.BottomRight  += FLineWidth.Width div 2 + 5;
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
    if PointInsideSegment(FVertexes[i], FVertexes[i+1], ADoublePoint, FLineWidth.Width) then
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

constructor TLine.Create(ADoublePoint: TDoublePoint; APenColor: TColor);
begin
  Inherited Create(APenColor);
  FStartPoint := ADoublePoint;
  FEndPoint := ADoublePoint;
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

procedure TLine.Move(ADisplacement: TDoublePoint);
begin
  FStartPoint += ADisplacement;
  FEndPoint += ADisplacement;
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
  Result := PointInsideSegment(FStartPoint, FEndPoint, ADoublePoint, FLineWidth.Width);
end;

{ TSelection }
procedure TSelection.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.Frame(WorldToDispCoord(FFigureBounds));
end;

constructor TSelection.Create(ADoublePoint: TDoublePoint);
begin
  Inherited Create(ADoublePoint, clBlack, clWhite);
  FLineWidth.SetValue(1);
  FLineStyle.SetValue(psDash);
end;

end.

