unit UParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, UTransform, StdCtrls,
  ExtCtrls, LCLClasses, Spin, FPCanvas;

type

  TLineWidthChange = procedure(AWidth: Integer) of Object;
  TLineStyleChange = procedure(ALineStyle: TFPPenStyle) of Object;
  TBrushStyleChange = procedure(ABrushStyle: TFPBrushStyle) of Object;
  TCornersNumberChange = procedure(ACornersNumber: Integer) of Object;
  TFactorChange = procedure(AFactor: Integer) of Object;

  { TParameter }

  TParameter = class
    FLabel: TLabel;
    FComponent: TControl;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLineWidthParameter }

  TLineWidthParameter = class(TParameter)
    FLineWidthChange: TLineWidthChange;
    procedure OnLineWidthChange(Sender: TObject);
    constructor Create(ALineWidthChange: TLineWidthChange);
  end;

  { TLineStyleParameter }

  TLineStyleParameter = class(TParameter)
    FLineStyleChange: TLineStyleChange;
    procedure OnDrawLineStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnLineStyleChange(Sender: TObject);
    constructor Create(ALineStyleChange: TLineStyleChange);
  end;

  { TBrushStyleParameter }

  TBrushStyleParameter = class(TParameter)
    FBrushStyleChange: TBrushStyleChange;
    procedure OnDrawBrushStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnBrushStyleChange(Sender: TObject);
    constructor Create(ABrushStyleChange: TBrushStyleChange);
  end;

  { TCornersNumberParameter }

  TCornersNumberParameter = class(TParameter)
    FCornersNumbersChange: TCornersNumberChange;
    procedure OnCornersNumberChange(Sender: TObject);
    constructor Create(ACornersNumbersChange: TCornersNumberChange);
  end;

  { TFactorParameter }

  TFactorParameter = class(TParameter)
    FFactorChange: TFactorChange;
    procedure OnFactorChange(Sender: TObject);
    constructor Create(ACaption: String; AFactorChange: TFactorChange;
      AValue: Integer);
  end;

  procedure ShowParams(APanel: TPanel);

var
  Params: array of TParameter;

implementation

uses
  LCLType;

{ Misc }

procedure ShowParams(APanel: TPanel);
var i: Integer;
begin
  for i := 0 to High(Params) do begin
    with Params[i] do begin
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

constructor TCornersNumberParameter.Create(
  ACornersNumbersChange: TCornersNumberChange);
begin
  Inherited Create;
  FCornersNumbersChange := ACornersNumbersChange;
  FLabel.Caption := 'Количество углов';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 15;
    MinValue := 3;
    Value := 3;
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

constructor TBrushStyleParameter.Create(ABrushStyleChange: TBrushStyleChange);
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
    ItemIndex := 0;
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

constructor TLineStyleParameter.Create(ALineStyleChange: TLineStyleChange);
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
    ItemIndex := 0;
    ItemHeight := 20;
    OnDrawItem := @OnDrawLineStyleItem;
    OnChange := @OnLineStyleChange;
  end;
end;

{ TParameter }

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

constructor TLineWidthParameter.Create(ALineWidthChange: TLineWidthChange);
begin
  Inherited Create;
  FLineWidthChange := ALineWidthChange;
  FLabel.Caption := 'Толщина линии';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 500;
    MinValue := 1;
    Value := 3;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    OnChange := @OnLineWidthChange;
  end;
end;

end.

