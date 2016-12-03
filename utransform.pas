unit UTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type

  TDoublePoint = record
    X: Double;
    Y: Double;
  end;

  TDoubleRect = record
    case Integer of
      0: (
          Left: Double;
          Top: Double;
          Right: Double;
          Bottom: Double;
        );
      1: (
          TopLeft: TDoublePoint;
          BottomRight: TDoublePoint;
        );
  end;

  TDimensions = record
    Height: Integer;
    Width: Integer;
  end;

  TArrayOfTpoint = array of TPoint;

operator + (ADblPointA, ADblPointB: TDoublePoint): TDoublePoint;
operator + (APoint: TPoint; ADblPoint: TDoublePoint): TDoublePoint;
operator + (AdblPoint: TDoublePoint; ANumber: Integer): TDoublePoint;

operator - (ADblPointA, ADblPointB: TDoublePoint): TDoublePoint;
operator - (APointA, APointB: TPoint): TPoint;
operator - (AdblPoint: TDoublePoint; ANumber: Integer): TDoublePoint;

operator * (ANumber: Double; ADblPoint: TDoublePoint): TDoublePoint;
operator * (ADblPoint: TDoublePoint; ANumber: Double): TDoublePoint;
operator * (ADblPointA, ADblPointB: TDoublePoint): Double;
operator * (APointA, APointB: TPoint): Double;

operator / (ADblPoint: TDoublePoint; ANumber: Double): TDoublePoint;

operator = (ADblPointA, ADblPointB: TDoublePoint): Boolean;

operator >= (ADblPointA, ADblPointB: TDoublePoint): Boolean;
operator <= (ADblPointA, ADblPointB: TDoublePoint): Boolean;

function DoublePoint(AX, AY: Double): TDoublePoint;
function DoublePoint(APoint: TPoint): TDoublePoint;
function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
procedure SetCanvasOffset(ACanvasOffset: TDoublePoint);
procedure SetCanvasOffset(AX, AY: Double);
procedure AddCanvasOffset(AX, AY: Double);
procedure AddCanvasOffset(ACanvasOffset: TDoublePoint);
function GetCanvasOffset: TDoublePoint;
function DispToWorldCoord(AX, AY: Integer): TDoublePoint;
function DispToWorldCoord(ARect: TRect): TDoubleRect;
function DispToWorldCoord(APoint: TPoint): TDoublePoint;
function WorldToDispCoord(ADoubleRect: TDoubleRect): TRect;
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
function WorldToDispDimension(ADimension: Double): Integer;
function WorldVertexesToDispCoord(AVertexes: array of TDoublePoint): TArrayOfTpoint;
procedure SetScale(AScale: Double);
function GetScale: Double;
procedure IncreaseScale;
procedure DecreaseScale;
function GetDispDimensions: TDimensions;
procedure SetDispDimensions(ADispDimensions: TDimensions);
function Dimensions(AWidth, AHeight: Integer): TDimensions;

property Scale: Double read GetScale write SetScale;
property DispDimensions: TDimensions
  read GetDispDimensions write SetDispDimensions;

implementation

const
  MaxScale = 16;
  MinScale = 0.0625;

var
  FScale: Double = 1.0;
  CanvasOffset: TDoublePoint;//инициализируется X:=0, Y:=0
  FDispDimensions:  TDimensions;

operator + (ADblPointA, ADblPointB: TDoublePoint): TDoublePoint;
begin
  Result.X := ADblPointA.X + ADblPointB.X;
  Result.Y := ADblPointA.Y + ADblPointB.Y;
end;

operator + (APoint: TPoint; ADblPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.x + ADblPoint.X;
  Result.Y := APoint.y + ADblPoint.Y;
end;

operator + (AdblPoint: TDoublePoint; ANumber: Integer): TDoublePoint;
begin
  Result.X := AdblPoint.X + ANumber;
  Result.Y := AdblPoint.Y + ANumber;
end;

operator - (ADblPointA, ADblPointB: TDoublePoint): TDoublePoint;
begin
  Result.X := ADblPointA.X - ADblPointB.X;
  Result.Y := ADblPointA.Y - ADblPointB.Y;
end;

operator - (APointA, APointB: TPoint): TPoint;
begin
  Result.X := APointA.X - APointB.X;
  Result.Y := APointA.Y - APointB.Y;
end;

operator - (AdblPoint: TDoublePoint; ANumber: Integer): TDoublePoint;
begin
  Result.X := AdblPoint.X - ANumber;
  Result.Y := AdblPoint.Y - ANumber;
end;

operator * (ANumber: Double; ADblPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := ADblPoint.X * ANumber;
  Result.Y := ADblPoint.Y * ANumber;
end;

operator * (ADblPoint: TDoublePoint; ANumber: Double): TDoublePoint;
begin
  Result.X := ADblPoint.X * ANumber;
  Result.Y := ADblPoint.Y * ANumber;
end;

operator * (ADblPointA, ADblPointB: TDoublePoint): Double;
begin
  Result := ADblPointA.X * ADblPointB.X + ADblPointA.Y * ADblPointB.Y;
end;

operator * (APointA, APointB: TPoint): Double;
begin
  Result := APointA.X * APointB.X + APointA.Y * APointB.Y;
end;

operator / (ADblPoint: TDoublePoint; ANumber: Double): TDoublePoint;
begin
  Result.X := ADblPoint.X / ANumber;
  Result.Y := ADblPoint.Y / ANumber;
end;

operator = (ADblPointA, ADblPointB: TDoublePoint): Boolean;
begin
  if (ADblPointA.X = ADblPointB.X) and (ADblPointA.Y = ADblPointB.Y) then Result := True
  else Result := False;
end;

operator >= (ADblPointA, ADblPointB: TDoublePoint): Boolean;
begin
  Result := (ADblPointA.X >= ADblPointB.X) and (ADblPointA.Y >= ADblPointB.Y);
end;

operator <= (ADblPointA, ADblPointB: TDoublePoint): Boolean;
begin
  Result := (ADblPointA.X <= ADblPointB.X) and (ADblPointA.Y <= ADblPointB.Y);
end;

{ TDoublePoint }
function DoublePoint(AX, AY: Double): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

function DoublePoint(APoint: TPoint): TDoublePoint;
begin
  Result := DoublePoint(APoint.x, APoint.y);
end;

{ TDoubleRect }
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
begin
  with Result do begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
begin
  with Result do begin
    TopLeft := ATopLeft;
    BottomRight := ABottomRight;
  end;
end;

{ Scale }
procedure SetScale(AScale: Double);
begin
  FScale := EnsureRange(AScale, MinScale, MaxScale);
end;

function GetScale: Double;
begin
  Result := FScale;
end;

procedure IncreaseScale;
begin
    Scale := Scale * 2;
end;

procedure DecreaseScale;
begin
    Scale := Scale / 2;
end;

function GetDispDimensions: TDimensions;
begin
  Result := FDispDimensions;
end;

procedure SetDispDimensions(ADispDimensions: TDimensions);
begin
  FDispDimensions := ADispDimensions;
end;

function Dimensions(AWidth, AHeight: Integer): TDimensions;
begin
  with Result do begin
    Width := AWidth;
    Height := AHeight;
  end;
end;

{ Canvas Offset }
procedure SetCanvasOffset(ACanvasOffset: TDoublePoint);
begin
  //можно ли сделать оператор присваивания CanvasOffset := ACanvasOffset
  CanvasOffset.X := ACanvasOffset.X;
  CanvasOffset.Y := ACanvasOffset.Y;
end;

procedure SetCanvasOffset(AX, AY: Double);
begin
  CanvasOffset.X := AX;
  CanvasOffset.Y := AY;
end;

procedure AddCanvasOffset(AX, AY: Double);
begin
  CanvasOffset.X += AX;
  CanvasOffset.Y += AY;
end;

procedure AddCanvasOffset(ACanvasOffset: TDoublePoint);
begin
  CanvasOffset += ACanvasOffset;
end;

function GetCanvasOffset: TDoublePoint;
begin
  Result := CanvasOffset;
end;

{ World -> Display }
function WorldToDispCoord(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    x := round(Scale * ADoublePoint.X - CanvasOffset.X);
    y := round(Scale * ADoublePoint.Y - CanvasOffset.Y);
  end;
end;

function WorldToDispCoord(ADoubleRect: TDoubleRect): TRect;
begin
  with Result do begin
    TopLeft := WorldToDispCoord(ADoubleRect.TopLeft);
    BottomRight := WorldToDispCoord(ADoubleRect.BottomRight);
  end;
end;

function WorldToDispDimension(ADimension: Double): Integer;
begin
  Result := round(ADimension * Scale);
end;

function WorldVertexesToDispCoord(
  AVertexes: array of TDoublePoint): TArrayOfTpoint;
var
  i: Integer;
  PointVertexes: TArrayOfTpoint;
begin
  SetLength(PointVertexes, Length(AVertexes));
  for i := 0 to High(AVertexes) do begin
    PointVertexes[i] := WorldToDispCoord(AVertexes[i]);
  end;
  Result := PointVertexes;
end;

{ Display -> World }
function DispToWorldCoord(AX, AY: Integer): TDoublePoint;
begin
  Result := DispToWorldCoord(Point(AX, AY));
end;

function DispToWorldCoord(APoint: TPoint): TDoublePoint;
begin
  Result := (APoint + CanvasOffset) / Scale;
end;

function DispToWorldCoord(ARect: TRect): TDoubleRect;
begin
  with Result do begin
    TopLeft := DispToWorldCoord(ARect.TopLeft);
    BottomRight := DispToWorldCoord(ARect.BottomRight);
  end;
end;

initialization

CanvasOffset.X := 0;
CanvasOffset.Y := 0;

end.

