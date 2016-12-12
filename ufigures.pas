unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Controls, UTransform, windows, Classes, SysUtils, Graphics, FPCanvas, LCL,
  math, UParameters;

type
  TParams = array of TParameter;
  { TFigure }

  TFigure = class
  private
    FGControl: TGraphicControl;
    FIsSelected: Boolean;
    FPenColor: TColor;
    FLineStyle: TFPPenStyle;
    FLineWidth: Integer;
    FParams: TParams;
    procedure DrawFigure(ACanvas: TCanvas); virtual; abstract;
    procedure AddParam(AParam: TParameter);
    procedure CreateParams; virtual;
    procedure ChangeLineWidth(AWidth: Integer);
    procedure ChangeLineStyle(ALineStyle: TFPPenStyle);
  public
    procedure Move(ADisplacement: TDoublePoint); virtual; abstract;
    function GetParams: TParams;
    property IsSelected: Boolean read FIsSelected write FIsSelected;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
    procedure Draw(AGControl: TGraphicControl); virtual;
    function GetBounds: TDoubleRect; virtual; abstract;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; virtual; abstract;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; virtual; abstract;
  end;

  { TPolyline }

  TPolyline = class(TFigure)
  private
    FVertexes: array of TDoublePoint;
    procedure DrawFigure(ACanvas: TCanvas); override;
  public
    procedure Move(ADisplacement: TDoublePoint); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer);
    procedure AddPoint(ADoublePoint: TDoublePoint);
    function GetBounds: TDoubleRect; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
  end;

  { TTwoPointFigure }

  TTwoPointFigure = class(TFigure)
  public
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); virtual; abstract;
    constructor Create(
      APenColor: TColor; APenStyle: TFPPenStyle; AThickness: Integer);
  end;

  { TFilledFigure }

  TFilledFigure = class(TTwoPointFigure)
  private
    FBrushColor: TColor;
    FBrushStyle: TFPBrushStyle;
    procedure CreateParams; override;
    procedure ChangeBrushStyle(ABrushStyle: TFPBrushStyle);
  public
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AThickness: Integer; AFillStyle: TFPBrushStyle);
    procedure Draw(AGControl: TGraphicControl); override;
  end;

  { TInscribedFigure }

  TInscribedFigure = class(TFilledFigure)
  private
    FFigureBounds: TDoubleRect;
  public
    procedure Move(ADisplacement: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle);
  end;

  { TRectangle }

  TRectangle = class(TInscribedFigure)
  private
    procedure DrawFigure(ACanvas: TCanvas); override;
  public
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TRoundRectangle }

  TRoundRectangle = class(TInscribedFigure)
  private
    FFactorX: Integer;
    FFactorY: Integer;
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure CreateParams; override;
    procedure ChangeXFactor(AFactor: Integer);
    procedure ChangeYFactor(AFactor: Integer);
  public
    constructor Create(ADoublePoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle;
      AFactorX, AFactorY: Integer);
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TLine }

  TLine = class(TTwoPointFigure)
  private
    FStartPoint: TDoublePoint;
    FEndPoint: TDoublePoint;
    procedure DrawFigure(ACanvas: TCanvas); override;
  public
    procedure Move(ADisplacement: TDoublePoint); override;
    constructor Create(AMousePos: TDoublePoint; APenColor: TColor;
      ALineStyle: TFPPenStyle; ALineWidth: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TEllipse }

  TEllipse = class(TInscribedFigure)
  private
    procedure DrawFigure(ACanvas: TCanvas); override;
  public
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  { TSelection }

  TSelection = class(TInscribedFigure)
  private
    procedure DrawFigure(ACanvas: TCanvas); override;
  public
    constructor Create(ADoublePoint: TDoublePoint);
  end;

  { TRegularPolygon }

  TRegularPolygon = class(TFilledFigure)
  private
    FCorners: Integer;
    FVertexes: array of TDoublePoint;
    FCenter: TDoublePoint;
    FCirclePoint: TDoublePoint;
    procedure DrawFigure(ACanvas: TCanvas); override;
    procedure CreateParams; override;
    procedure ChangeCornersNumber(ACorners: Integer);
    procedure UpdateFigure;
  public
    procedure Move(ADisplacement: TDoublePoint); override;
    constructor Create(ACenterPoint: TDoublePoint; APenColor, ABrushColor: TColor;
      APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle; ACorners: Integer);
    procedure SetSecondPoint(ADoublePoint: TDoublePoint); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ADoubleRect: TDoubleRect): Boolean; override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
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

procedure TRoundRectangle.CreateParams;
begin
  Inherited;
  AddParam(TFactorParameter.Create('Скругление по X: ', @ChangeXFactor, FFactorX));
  AddParam(TFactorParameter.Create('Скругление по Y: ', @ChangeYFactor, FFactorY));
end;

procedure TRoundRectangle.ChangeXFactor(AFactor: Integer);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      (Figures[i] as TRoundRectangle).FFactorX := AFactor;
  end;
  FGControl.Invalidate;
end;

procedure TRoundRectangle.ChangeYFactor(AFactor: Integer);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      (Figures[i] as TRoundRectangle).FFactorY := AFactor;
  end;
  FGControl.Invalidate;
end;

function TRoundRectangle.IsIntersect(ADoubleRect: TDoubleRect): Boolean;
var
  RoundRect: HRGN;
begin
  with WorldToDispCoord(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FFactorX, FFactorY);
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
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, FFactorY, FFactorY);
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
begin
  UpdateFigure;
  ACanvas.Polygon(WorldVertexesToDispCoord(FVertexes));
end;

procedure TRegularPolygon.CreateParams;
begin
  Inherited;
  AddParam(TCornersNumberParameter.Create(@ChangeCornersNumber, FCorners));
end;

procedure TRegularPolygon.ChangeCornersNumber(ACorners: Integer);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      (Figures[i] as TRegularPolygon).FCorners := ACorners;
  end;
  FGControl.Invalidate;
end;

procedure TRegularPolygon.UpdateFigure;
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

procedure TFilledFigure.CreateParams;
begin
  Inherited;
  AddParam(TBrushStyleParameter.Create(@ChangeBrushStyle, FBrushStyle));
end;

procedure TFilledFigure.ChangeBrushStyle(ABrushStyle: TFPBrushStyle);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      (Figures[i] as TFilledFigure).FBrushStyle := ABrushStyle;
  end;
  FGControl.Invalidate;
end;

constructor TFilledFigure.Create(APenColor, ABrushColor: TColor;
  APenStyle: TFPPenStyle; AThickness: Integer; AFillStyle: TFPBrushStyle);
begin
  Inherited Create(APenColor, APenStyle, AThickness);
  FBrushColor := ABrushColor;
  FBrushStyle := AFillStyle;
end;

procedure TFilledFigure.Draw(AGControl: TGraphicControl);
begin
  with AGControl.Canvas do begin
    Brush.Color := FBrushColor;
    Brush.Style := FBrushStyle;
  end;
  Inherited;
end;

procedure TFigure.AddParam(AParam: TParameter);
begin
  SetLength(FParams, Length(FParams) + 1);
  FParams[High(FParams)] := AParam;
end;

procedure TFigure.CreateParams;
begin
  AddParam(TLineWidthParameter.Create(@ChangeLineWidth, FLineWidth));
  AddParam(TLineStyleParameter.Create(@ChangeLineStyle, FLineStyle));
end;



procedure TFigure.ChangeLineWidth(AWidth: Integer);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      Figures[i].FLineWidth := AWidth;
  end;
  FGControl.Invalidate;
end;

procedure TFigure.ChangeLineStyle(ALineStyle: TFPPenStyle);
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
    if Figures[i].IsSelected then
      Figures[i].FLineStyle := ALineStyle;
  end;
  FGControl.Invalidate;
end;

function TFigure.GetParams: TParams;
var i: Integer;
begin
  for i := Low(FParams) to High(FParams ) do FParams[i].Free;
  FParams := Nil;
  CreateParams;
  Result := FParams;
end;

{ TFigure }
constructor TFigure.Create(APenColor: TColor; APenStyle: TFPPenStyle;
  AThickness: Integer);
begin
  FPenColor := APenColor;
  FLineWidth := AThickness;
  FLineStyle := APenStyle;
end;

procedure TFigure.Draw(AGControl: TGraphicControl);
var FrameCoords: TRect;
begin
  FGControl := AGControl;
  with AGControl.Canvas, GetBounds do begin
    if IsSelected then begin
      Pen.Style := psDash;
      Pen.Color := clRed;
      Pen.Width := 3;
      FrameCoords := WorldToDispCoord(DoubleRect(TopLeft, BottomRight));
      FrameCoords.TopLeft -= FLineWidth div 2 + 5;
      FrameCoords.BottomRight  += FLineWidth div 2 + 5;
      Frame(FrameCoords);
    end;
    Pen.Color := FPenColor;
    Pen.Width := FLineWidth;
    Pen.Style := FLineStyle;
  end;
  DrawFigure(AGControl.Canvas);
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
    if PointInsideSegment(FVertexes[i], FVertexes[i+1], ADoublePoint, FLineWidth) then
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
  Result := PointInsideSegment(FStartPoint, FEndPoint, ADoublePoint, FLineWidth);
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

