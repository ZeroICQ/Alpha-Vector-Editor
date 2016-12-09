unit UParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, UTransform, StdCtrls,
  ExtCtrls, Spin, FPCanvas;

type

  TLineWidthChange = procedure(AWidth: Integer) of Object;
  TLineStyleChange = procedure(ALineStyle: TFPPenStyle) of Object;
  TBrushStyleChange = procedure(ABrushStyle: TFPBrushStyle) of Object;
  TCornersNumberChange = procedure(ACornersNumber: Integer) of Object;
  TFactorChange = procedure(AFactor: Integer) of Object;

  { TParameter }

  TParameter = class
  private
    FLabel: TLabel;
    FComponent: TControl;
  public
    procedure SetValue(AWidth: Integer); virtual; abstract;
    procedure SetEmpty; virtual; abstract;
    function GetValue: Integer; virtual; abstract;
    procedure Hide;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLineWidthParameter }

  TLineWidthParameter = class(TParameter)
  private
    FLineWidthChange: TLineWidthChange;
    procedure OnLineWidthChange(Sender: TObject);
  public
    procedure SetValue(AWidth: Integer); override;
    procedure SetEmpty; override;
    function GetValue: Integer; override;
    constructor Create(ALineWidthChange: TLineWidthChange; AWidth: Integer);
  end;

  { TLineStyleParameter }

  TLineStyleParameter = class(TParameter)
  private
    FLineStyleChange: TLineStyleChange;
    procedure OnDrawLineStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnLineStyleChange(Sender: TObject);
  public
    procedure SetValue(ALineStyle: Integer); override;
    procedure SetEmpty; override;
    function GetValue: Integer; override;
    constructor Create(ALineStyleChange: TLineStyleChange; AStyle: TFPPenStyle);
  end;

  { TBrushStyleParameter }

  TBrushStyleParameter = class(TParameter)
  private
    FBrushStyleChange: TBrushStyleChange;
    procedure OnDrawBrushStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnBrushStyleChange(Sender: TObject);
  public
    procedure SetValue(ABrushStyle: Integer); override;
    procedure SetEmpty; override;
    function GetValue: Integer; override;
    constructor Create(ABrushStyleChange: TBrushStyleChange; AStyle: TFPBrushStyle);
  end;

  { TCornersNumberParameter }

  TCornersNumberParameter = class(TParameter)
  private
    FCornersNumbersChange: TCornersNumberChange;
    procedure OnCornersNumberChange(Sender: TObject);
  public
    procedure SetValue(ACorners: Integer); override;
    procedure SetEmpty; override;
    function GetValue: Integer; override;
    constructor Create(ACornersNumbersChange: TCornersNumberChange; ACorners: Integer);
  end;

  { TFactorParameter }

  TFactorParameter = class(TParameter)
  private
    FFactorChange: TFactorChange;
  public
    procedure SetValue(AFactor: Integer); override;
    procedure SetEmpty; override;
    function GetValue: Integer; override;
    procedure OnFactorChange(Sender: TObject);
    constructor Create(ACaption: String; AFactorChange: TFactorChange;
      AValue: Integer);
  end;

  procedure ShowParams(AParams: array of TParameter; APanel: TPanel);

var
  Params: array of TParameter;

implementation

uses
  LCLType;

{ Misc }

procedure ShowParams(AParams: array of TParameter; APanel: TPanel);
var i: Integer;
begin
  for i := Low(AParams) to High(AParams) do begin
    with AParams[i] do begin
      FLabel.Top := i * 50;
      FLabel.Left := 2;
      FLabel.Parent := APanel;
      FComponent.Top := i * 50 + FLabel.ClientHeight + 5;
      FComponent.Left := APanel.ClientWidth - FComponent.ClientWidth;
      FComponent.Parent := APanel;
    end;
  end;
end;

{ TFactorParameter }

procedure TFactorParameter.SetValue(AFactor: Integer);
begin
  FFactorChange(AFactor);
end;

procedure TFactorParameter.SetEmpty;
begin
  with FComponent as TSpinEdit do begin
    Value := MinValue;
  end;
end;

function TFactorParameter.GetValue: Integer;
begin
  Result := (FComponent as TSpinEdit).Value;
end;

procedure TFactorParameter.OnFactorChange(Sender: TObject);
begin
  FFactorChange((Sender as TSpinEdit).Value);
end;

constructor TFactorParameter.Create(ACaption: String;
  AFactorChange: TFactorChange; AValue: Integer);
begin
  Inherited Create;
  FFactorChange := AFactorChange;
  FLabel.Caption := ACaption;
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 1000;
    MinValue := 1;
    Value := AValue;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 130;
    OnChange := @OnFactorChange;
  end;
end;

{ TCornersNumberParameter }

procedure TCornersNumberParameter.OnCornersNumberChange(Sender: TObject);
begin
  with Sender as TSpinEdit do begin
    FCornersNumbersChange(Value);
  end;
end;

procedure TCornersNumberParameter.SetValue(ACorners: Integer);
begin
  FCornersNumbersChange(ACorners);
end;

procedure TCornersNumberParameter.SetEmpty;
begin
  with FComponent as TSpinEdit do begin
    Value := MinValue;
  end;
end;

function TCornersNumberParameter.GetValue: Integer;
begin
  Result := (FComponent as TSpinEdit).Value;
end;

constructor TCornersNumberParameter.Create(
  ACornersNumbersChange: TCornersNumberChange; ACorners: Integer);
begin
  Inherited Create;
  FCornersNumbersChange := ACornersNumbersChange;
  FLabel.Caption := 'Количество углов';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 15;
    MinValue := 3;
    Value := ACorners;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    OnChange := @OnCornersNumberChange;
  end;
end;

{ TBrushStyleParameter }

procedure TBrushStyleParameter.OnDrawBrushStyleItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    ARect.Top += 3;
    ARect.Left += 3;
    ARect.Right -= 3;
    ARect.Bottom -= 3;
    Rectangle(ARect);
    //workaround of not working bsClear
    if TFPBrushStyle(Index) = bsClear then begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
    end
    else begin
      Brush.Style := TFPBrushStyle(Index);
      Brush.Color := clBlack;
    end;
    if odFocused in State then
      Brush.Color := clBlue;
    Pen.Color := clBlack;
    Rectangle(ARect);
  end;
end;

procedure TBrushStyleParameter.OnBrushStyleChange(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FBrushStyleChange(TFPBrushStyle(ItemIndex));
  end;
end;

procedure TBrushStyleParameter.SetValue(ABrushStyle: Integer);
begin
  FBrushStyleChange(TFPBrushStyle(ABrushStyle));
end;

procedure TBrushStyleParameter.SetEmpty;
begin
  with FComponent as TComboBox do begin
      ItemIndex := 0;
  end;
end;

function TBrushStyleParameter.GetValue: Integer;
begin
  Result := (FComponent as TComboBox).ItemIndex;
end;

constructor TBrushStyleParameter.Create(ABrushStyleChange: TBrushStyleChange;
  AStyle: TFPBrushStyle);
var i: Integer;
begin
  Inherited Create;
  FBrushStyleChange := ABrushStyleChange;
  FLabel.Caption := 'Стиль заливки';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    for i := 0 to 7 do Items.Add('');
    Font.Size := 10;
    Width := 130;
    ItemIndex := ord(AStyle);
    ReadOnly := True;
    AutoSelect := False;
    ItemHeight := 20;
    Style := csOwnerDrawFixed;
    OnDrawItem := @OnDrawBrushStyleItem;
    OnChange := @OnBrushStyleChange;
  end;
end;

{ TLineStyleParameter }

procedure TLineStyleParameter.OnDrawLineStyleItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Style := psClear;
    Pen.Color := clWhite;
    ARect.Top += 3;
    ARect.Left += 3;
    ARect.Right -= 3;
    ARect.Bottom -= 3;
    if odFocused in State then
      Brush.Color := clBlue;
    Rectangle(ARect);
    Pen.Style := TFPPenStyle(Index);
    Pen.Width := 3;
    Pen.Color := clBlack;
    Line(ARect);
  end;
end;

procedure TLineStyleParameter.OnLineStyleChange(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyleChange(TFPPenStyle(ItemIndex));
  end;
end;

procedure TLineStyleParameter.SetValue(ALineStyle: Integer);
begin
  FLineStyleChange(TFPPenStyle(ALineStyle));
end;

procedure TLineStyleParameter.SetEmpty;
begin
  with FComponent as TComboBox do begin
    ItemIndex := 0;
  end;
end;

function TLineStyleParameter.GetValue: Integer;
begin
  Result := (FComponent as TComboBox).ItemIndex;
end;

constructor TLineStyleParameter.Create(ALineStyleChange: TLineStyleChange;
  AStyle: TFPPenStyle);
var i: Integer;
begin
  Inherited Create;
  FLineStyleChange := ALineStyleChange;
  FLabel.Caption := 'Стиль линии';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    for i := 0 to 4 do Items.Add('');
    Style := csOwnerDrawFixed;
    ReadOnly := True;
    Width := 130;
    ItemIndex := ord(AStyle);
    ItemHeight := 20;
    OnDrawItem := @OnDrawLineStyleItem;
    OnChange := @OnLineStyleChange;
  end;
end;

{ TParameter }

procedure TParameter.Hide;
begin
  FComponent.Parent := nil;
  FLabel.Parent := nil;
end;

constructor TParameter.Create;
begin
  Inherited;
  FLabel := TLabel.Create(nil);
  FLabel.Font.Size := 11;
end;

destructor TParameter.Destroy;
begin
  FLabel.Free;
  FComponent.Free;
  inherited Destroy;
end;

{ TLineWidthParameter }

procedure TLineWidthParameter.OnLineWidthChange(Sender: TObject);
begin
  FLineWidthChange((Sender as TSpinEdit).Value);
end;

procedure TLineWidthParameter.SetValue(AWidth: Integer);
begin
  (FComponent as TSpinEdit).Value := AWidth;
  FLineWidthChange(AWidth);
end;

procedure TLineWidthParameter.SetEmpty;
begin
  with FComponent as TSpinEdit do begin
    Value := MinValue;
  end;
end;

function TLineWidthParameter.GetValue: Integer;
begin
  Result := (FComponent as TSpinEdit).Value;
end;

constructor TLineWidthParameter.Create(ALineWidthChange: TLineWidthChange; AWidth: Integer);
begin
  Inherited Create;
  FLineWidthChange := ALineWidthChange;
  FLabel.Caption := 'Толщина линии';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 500;
    MinValue := 1;
    Value := AWidth;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    OnChange := @OnLineWidthChange;
  end;
end;

end.

