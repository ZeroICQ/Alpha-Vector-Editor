unit UParameters;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FPCanvas, Graphics, UTransform, typinfo;

type

  TParamClass = class of TParam;

  { TParam }

  TParam = class
  public
    procedure Apply(ACanvas: TCanvas); virtual;
    function GetIntValue: Integer; virtual; abstract;
    function Compare(AParam: TParam): Boolean; virtual; abstract;
  end;

  { TParamLineWidth }

  TParamLineWidth = class(TParam)
  private
    FLineWidth: Integer;
  public
    property Width: Integer read FLineWidth write FLineWidth;
    procedure Apply(ACanvas: TCanvas); override;
    procedure SetValue(AValue: Integer);
    function GetIntValue: Integer; override;
    function Compare(AParam: TParam): Boolean; override;
  published
    property Value: Integer read FLineWidth write FLineWidth;
    function GetStrValue: String;
  end;

  { TParamLineStyle }

  TParamLineStyle = class(TParam)
  private
    FLineStyle: TFPPenStyle;
  public
    property Style: TFPPenStyle read FLineStyle write FLineStyle;
    procedure Apply(ACanvas: TCanvas); override;
    function GetIntValue: Integer; override;
    procedure SetValue(ALineStyle: TFPPenStyle);
    function Compare(AParam: TParam): Boolean; override;
  published
    property Value: TFPPenStyle read FLineStyle write FLineStyle;
    function GetStrValue: String;
  end;

  { TParamBrushStyle }

  TParamBrushStyle = class(TParam)
  private
    FBrushStyle: TFPBrushStyle;
  public
    property Style: TFPBrushStyle read FBrushStyle write FBrushStyle;
    procedure Apply(ACanvas: TCanvas); override;
    procedure SetValue(ABrushStyle: TFPBrushStyle);
    function GetIntValue: Integer; override;
    function Compare(AParam: TParam): Boolean; override;
  published
    property Value: TFPBrushStyle read FBrushStyle write FBrushStyle;
    function GetStrValue: String;
  end;

  { TParamCorners }

  TParamCorners = class(TParam)
  private
    FCorners: Integer;
  public
    property Corners: Integer read FCorners write FCorners;
    procedure SetValue(AValue: Integer);
    function GetIntValue: Integer; override;
    function Compare(AParam: TParam): Boolean; override;
  published
    property Value: Integer read FCorners write FCorners;
    function GetStrValue: String;
  end;

  { TParamInteger }

  TParamInteger = class(TParam)
  private
    FValue: Integer;
  public
    procedure SetValue(AValue: Integer);
    function GetIntValue: Integer; override;
    function Compare(AParam: TParam): Boolean; override;
  published
    property Value: Integer read FValue write FValue;
    function GetStrValue: String;
  end;

  { TParamXCoeff }

  TParamXCoeff = class(TParamInteger)
  end;

  { TParamYCoeff }

  TParamYCoeff = class(TParamInteger)
  end;

  TParamArr = array of TParam;

implementation


{ TParamInteger }

procedure TParamInteger.SetValue(AValue: Integer);
begin
  Value := AValue;
end;

function TParamInteger.GetIntValue: Integer;
begin
  Result := Value;
end;

function TParamInteger.Compare(AParam: TParam): Boolean;
begin
  Result := Value = (AParam as TParamInteger).Value;
end;

function TParamInteger.GetStrValue: String;
begin
  Result := IntToStr(Value);
end;

{ TParamCorners }

procedure TParamCorners.SetValue(AValue: Integer);
begin
  Corners := AValue;
end;

function TParamCorners.GetIntValue: Integer;
begin
  Result := FCorners;
end;

function TParamCorners.Compare(AParam: TParam): Boolean;
begin
  Result := Corners = (AParam as TParamCorners).Corners;
end;

function TParamCorners.GetStrValue: String;
begin
  Result := IntToStr(Corners);
end;

{ TParam }

procedure TParam.Apply(ACanvas: TCanvas);
begin
  //заглушка
end;

{ TParamBrushStyle }

procedure TParamBrushStyle.Apply(ACanvas: TCanvas);
begin
  ACanvas.Brush.Style := Style;
end;

procedure TParamBrushStyle.SetValue(ABrushStyle: TFPBrushStyle);
begin
  Style := ABrushStyle;
end;

function TParamBrushStyle.GetIntValue: Integer;
begin
  Result := Ord(Style);
end;

function TParamBrushStyle.Compare(AParam: TParam): Boolean;
begin
  Result := Style = (AParam as TParamBrushStyle).Style;
end;

function TParamBrushStyle.GetStrValue: String;
begin
  Result := GetEnumName(TypeInfo(Style), ord(Style));
end;

{ TParamLineStyle }

procedure TParamLineStyle.Apply(ACanvas: TCanvas);
begin
  ACanvas.Pen.Style := Style;
end;

procedure TParamLineStyle.SetValue(ALineStyle: TFPPenStyle);
begin
  FLineStyle := ALineStyle;
end;

function TParamLineStyle.GetIntValue: Integer;
begin
  Result := Ord(Style);
end;

function TParamLineStyle.GetStrValue: String;
begin
  Result := GetEnumName(TypeInfo(Style), ord(Style));
end;

function TParamLineStyle.Compare(AParam: TParam): Boolean;
begin
  Result := Style = (AParam as TParamLineStyle).Style;
end;

{ TParamLineWidth }

procedure TParamLineWidth.Apply(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width := Width;
end;

procedure TParamLineWidth.SetValue(AValue: Integer);
begin
  Width := AValue;
end;

function TParamLineWidth.GetIntValue: Integer;
begin
  Result := Width;
end;

function TParamLineWidth.GetStrValue: String;
begin
  Result := IntToStr(Width);
end;

function TParamLineWidth.Compare(AParam: TParam): Boolean;
begin
  Result:= Width = (AParam as TParamLineWidth).Width ;
end;

end.
