unit UParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas, Graphics;

type

  TParamClass = class of TParam;

  { TParam }

  TParam = class
  public
    procedure Apply(ACanvas: TCanvas); virtual;
    procedure SetValue(AValue: Integer); virtual;
    procedure SetValue(ALineStyle: TFPPenStyle); virtual;
    procedure SetValue(ABrushStyle: TFPBrushStyle); virtual;
  end;

  { TParamLineWidth }

  TParamLineWidth = class(TParam)
  private
    FLineWidth: Integer;
  public
    property Width: Integer read FLineWidth write FLineWidth;
    procedure Apply(ACanvas: TCanvas); override;
    procedure SetValue(AValue: Integer); override;
  end;

  { TParamLineStyle }

  TParamLineStyle = class(TParam)
  private
    FLineStyle: TFPPenStyle;
  public
    property Style: TFPPenStyle read FLineStyle write FLineStyle;
    procedure Apply(ACanvas: TCanvas); override;
    procedure SetValue(ALineStyle: TFPPenStyle); override;
  end;

  { TParamBrushStyle }

  TParamBrushStyle = class(TParam)
  private
    FBrushStyle: TFPBrushStyle;
  public
    property Style: TFPBrushStyle read FBrushStyle write FBrushStyle;
    procedure Apply(ACanvas: TCanvas); override;
    procedure SetValue(ABrushStyle: TFPBrushStyle); override;
  end;

  { TParamCorners }

  TParamCorners = class(TParam)
  private
    FCorners: Integer;
  public
    property Corners: Integer read FCorners write FCorners;
    procedure SetValue(AValue: Integer); override;

  end;

  { TParamInteger }

  TParamInteger = class(TParam)
  private
    FValue: Integer;
  public
    property Value: Integer read FValue write FValue;
    procedure SetValue(AValue: Integer); override;
  end;

  TParamArr = array of TParam;

implementation

{ TParamInteger }

procedure TParamInteger.SetValue(AValue: Integer);
begin
  Value := AValue;
end;

{ TParamCorners }

procedure TParamCorners.SetValue(AValue: Integer);
begin
  Corners := AValue;
end;

{ TParam }

procedure TParam.Apply(ACanvas: TCanvas);
begin
  //заглушка
end;

procedure TParam.SetValue(AValue: Integer);
begin
  //заглушка
end;

procedure TParam.SetValue(ALineStyle: TFPPenStyle);
begin
  //заглушка
end;

procedure TParam.SetValue(ABrushStyle: TFPBrushStyle);
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

{ TParamLineStyle }

procedure TParamLineStyle.Apply(ACanvas: TCanvas);
begin
  ACanvas.Pen.Style := Style;
end;

procedure TParamLineStyle.SetValue(ALineStyle: TFPPenStyle);
begin
  FLineStyle := ALineStyle;
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

end.
