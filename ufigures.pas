unit UFigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Controls, UTransform, windows, Classes, SysUtils, Graphics, FPCanvas, LCL,
  math, UParameters, UAppState;

type
  { TFigure }

  TFigure = class(TPersistent)
  private
    FIsSelected: Boolean;
    FParams: array of TParam;
    FPenColor: TColor;
    FLineWidth: TParamLineWidth;
    FLineStyle: TParamLineStyle;
  public
    function GetStrCoord: String; virtual; abstract;
    procedure SetCoord(ACoords: TTwoDStrArr); virtual; abstract;
    property IsSelected: Boolean read FIsSelected write FIsSelected;
    procedure DrawFigure(ACanvas: TCanvas); virtual; abstract;
    procedure AddParam(AParam: TParam);
    function GetParams: TParamArr;
    procedure Move(ADisplacement: TDoublePoint); virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual;
    function GetBounds: TDoubleRect; virtual; abstract;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; virtual; abstract;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; virtual; abstract;
    procedure CreateParams;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); virtual;
    constructor Create(APenColor: TColor);
  published
    property FParamLineWidth: TParamLineWidth read FLineWidth write FLineWidth;
    property FParamLineStyle: TParamLineStyle read FLineStyle write FLineStyle;
    property FParamPenColor: TColor read FPenColor write FPenColor;
  end;

  { TPolyline }

  TPolyline = class(TFigure)
  private
    FVertexes: array of TDoublePoint;
  public
    function GetStrCoord: String; override;
    procedure SetCoord(ACoords: TTwoDStrArr); override;
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Move(ADisplacement: TDoublePoint); override;
    procedure AddPoint(ADoublePoint: TDoublePoint);
    function GetBounds: TDoubleRect; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); override;
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
    procedure CreateParams;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); override;
    constructor Create(APenColor, ABrushColor: TColor);
  published
    property FParamBrushStyle: TParamBrushStyle read FBrushStyle write FBrushStyle;
    property FParamBrushColor: TColor read FBrushColor write FBrushColor;
  end;

  { TInscribedFigure }

  TInscribedFigure = class(TFilledFigure)
  private
    FFigureBounds: TDoubleRect;
  public
    function GetStrCoord: String; override;
    procedure SetCoord(ACoords: TTwoDStrArr); override;
    property FParamFigureBounds: TDoubleRect read FFigureBounds write FFigureBounds;
    procedure Move(ADisplacement: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); override;
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
    FXCoeff: TParamXCoeff;
    FYCoeff: TParamYCoeff;
  public
    procedure DrawFigure(ACanvas: TCanvas); override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    procedure CreateParams;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor);
  published
    property FParamXCoeff: TParamXCoeff read FXCoeff write FXCoeff;
    property FParamYCoeff: TParamYCoeff read FYCoeff write FYCoeff;
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
  public
    FStartPoint: TDoublePoint;
    FEndPoint: TDoublePoint;
  public
    function GetStrCoord: String; override;
    procedure SetCoord(ACoords: TTwoDStrArr); override;
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Move(ADisplacement: TDoublePoint); override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor);
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
    FCorners: TParamCorners;
    FVertexes: array of TDoublePoint;
    FCenter: TDoublePoint;
    FCirclePoint: TDoublePoint;
  public
    function GetStrCoord: String; override;
    procedure SetCoord(ACoords: TTwoDStrArr); override;
    procedure UpdateFigure;
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure Move(ADisplacement: TDoublePoint); override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    procedure CreateParams;
    constructor CreateFromCoords(ACoords: TTwoDStrArr); override;
    constructor Create(ACenterPoint: TDoublePoint; APenColor, ABrushColor: TColor);
  published
    property FParamCorners: TParamCorners read FCorners write FCorners ;
  end;

  TFigureArr = array of TFigure;
  TFigureClass = class of TFigure;
var
  Figures: TFigureArr;

implementation

{ Misc }

function FormatPoint(ADoublePoint: TDoublePoint): String;
begin
  Result := Format('%F:%F;',[ADoublePoint.X, ADoublePoint.Y])
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
  CreateParams;
end;

procedure TRoundRectangle.DrawFigure(ACanvas: TCanvas);
begin
  ACanvas.RoundRect(WorldToDispCoord(FParamFigureBounds), FParamXCoeff.Value, FParamYCoeff.Value);
end;

function TRoundRectangle.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  RoundRect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FParamXCoeff.Value, FParamYCoeff.Value);
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
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FParamXCoeff.Value, FParamYCoeff.Value);
  end;
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(RoundRect, Point.X, Point.Y);
  DeleteObject(RoundRect);
end;

procedure TRoundRectangle.CreateParams;
begin
  FParamXCoeff := TParamXCoeff.Create;
  AddParam(FParamXCoeff);
  FParamYCoeff := TParamYCoeff.Create;
  AddParam(FParamYCoeff);
end;

constructor TRoundRectangle.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  Inherited;
  CreateParams;
end;


{ TInscribedFigure }

function TInscribedFigure.GetStrCoord: String;
begin
  Result := Concat(FormatPoint(GetBounds.TopLeft), FormatPoint(GetBounds.BottomRight));
end;

procedure TInscribedFigure.SetCoord(ACoords: TTwoDStrArr);
begin
  FFigureBounds := DoubleRect(
    StrToFloat(ACoords[0,0]), StrToFloat(ACoords[0,1]),
    StrToFloat(ACoords[1,0]), StrToFloat(ACoords[1,1]));
end;

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

constructor TInscribedFigure.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  Inherited;
  if Length(ACoords) > 2 then Exit;
  FFigureBounds.Left := StrToFloat(ACoords[0, 0]);
  FFigureBounds.Top := StrToFloat(ACoords[0, 1]);
  FFigureBounds.Right := StrToFloat(ACoords[1, 0]);
  FFigureBounds.Bottom := StrToFloat(ACoords[1, 1]);
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
  FCenter := ACenterPoint;
  FCirclePoint := ACenterPoint + 1;
  CreateParams;
end;

procedure TRegularPolygon.DrawFigure(ACanvas: TCanvas);
begin
  UpdateFigure;
  ACanvas.Polygon(WorldVertexesToDispCoord(FVertexes));
end;

function TRegularPolygon.GetStrCoord: String;
begin
  Result := Concat(FormatPoint(FCenter), FormatPoint(FCirclePoint));
end;

procedure TRegularPolygon.SetCoord(ACoords: TTwoDStrArr);
begin
  FCenter := DoublePoint(StrToFloat(ACoords[0,0]), StrToFloat(ACoords[0,1]));
  FCirclePoint := DoublePoint(StrToFloat(ACoords[1,0]), StrToFloat(ACoords[1,1]));
end;

procedure TRegularPolygon.UpdateFigure;
var
  i: Integer;
  Radius: Double;
  TurnAngle: Double;
begin
  Radius := sqrt((FCirclePoint.X - FCenter.X)**2 + (FCirclePoint.Y - FCenter.Y)**2);
  { TODO : Улучшить алгоритм }
  SetLength(FVertexes, FParamCorners.Corners);
  TurnAngle := GetTurnAngle(FCenter, FCirclePoint);
  for i := 0 to FParamCorners.Corners - 1 do begin
    FVertexes[i].x := FCenter.X + (Radius*sin((i * 2 * pi / FParamCorners.Corners) - TurnAngle));
    FVertexes[i].y := FCenter.Y + (Radius*cos((i * 2 * pi / FParamCorners.Corners) - TurnAngle));
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
  SetLength(Points, FParamCorners.Corners);
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
  SetLength(Points, FParamCorners.Corners);
  Points := WorldVertexesToDispCoord(FVertexes);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Point := WorldToDispCoord(ADoublePoint);
  Result := PtInRegion(Polygon, Point.x, Point.y);
  DeleteObject(Polygon);
end;

procedure TRegularPolygon.CreateParams;
begin
  FParamCorners := TParamCorners.Create;
  AddParam(FParamCorners);
end;

constructor TRegularPolygon.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  Inherited;
  if Length(ACoords) > 2 then Exit;
  FCenter := DoublePoint(StrToFloat(ACoords[0, 0]), StrToFloat(ACoords[0, 1]));
  FCirclePoint := DoublePoint(StrToFloat(ACoords[1, 0]), StrToFloat(ACoords[1, 1]));
  CreateParams;
end;

{ TFilledFigure }

constructor TFilledFigure.Create(APenColor, ABrushColor: TColor);
begin
  Inherited Create(APenColor);
  FParamBrushColor := ABrushColor;
  CreateParams;
end;

procedure TFilledFigure.Draw(ACanvas: TCanvas);
begin
  with ACanvas do begin
    Brush.Color := FParamBrushColor;
    Brush.Style := FParamBrushStyle.Style;
  end;
  Inherited;
end;

procedure TFilledFigure.CreateParams;
begin
  FParamBrushStyle := TParamBrushStyle.Create;
  AddParam(FParamBrushStyle);
end;

constructor TFilledFigure.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  Inherited;
  CreateParams;
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
  FPenColor := APenColor;
  CreateParams;
  if Self.ClassType <> TSelection then SetAppStateModified;
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

procedure TFigure.CreateParams;
begin
  IsSelected := False;
  FParamLineWidth := TParamLineWidth.Create;
  AddParam(FParamLineWidth);
  FParamLineStyle := TParamLineStyle.Create;
  AddParam(FParamLineStyle);
end;

constructor TFigure.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  CreateParams;
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

function TPolyline.GetStrCoord: String;
var
  i: Integer;
  CoordStr: String = '';
begin
  for i := Low(FVertexes) to High(FVertexes) do
    CoordStr += FormatPoint(FVertexes[i]);

  Result := CoordStr;
end;

procedure TPolyline.SetCoord(ACoords: TTwoDStrArr);
var i: Integer;
begin
  for i := Low(ACoords) to High(ACoords) do begin
    AddPoint(DoublePoint(StrToFloat(ACoords[i, 0]), StrToFloat(ACoords[i, 1])));
  end;
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

constructor TPolyline.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  Inherited;
  SetCoord(ACoords);
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

function TLine.GetStrCoord: String;
var
  CoordStr: String = '';
begin
  Result := Concat(FormatPoint(FStartPoint), FormatPoint(FEndPoint));
end;

procedure TLine.SetCoord(ACoords: TTwoDStrArr);
begin
  FStartPoint := DoublePoint(StrToFloat(ACoords[0,0]), StrToFloat(ACoords[0,1]));
  FEndPoint := DoublePoint(StrToFloat(ACoords[1,0]), StrToFloat(ACoords[1,1]));
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
  Result := PointInsideSegment(FStartPoint, FEndPoint, ADoublePoint, FParamLineWidth.Width);
end;

constructor TLine.CreateFromCoords(ACoords: TTwoDStrArr);
begin
  Inherited;
  if Length(ACoords) > 2 then Exit; //лучше бы ошибку кидать, но времени нет
  FStartPoint := DoublePoint(StrToFloat(ACoords[0, 0]), StrToFloat(ACoords[0, 1]));
  FEndPoint := DoublePoint(StrToFloat(ACoords[1, 0]), StrToFloat(ACoords[1, 1]));
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

initialization

RegisterClass(TFigure);
RegisterClass(TPolyline);
RegisterClass(TRectangle);
RegisterClass(TRoundRectangle);
RegisterClass(TLine);
RegisterClass(TEllipse);
RegisterClass(TRegularPolygon);

end.

