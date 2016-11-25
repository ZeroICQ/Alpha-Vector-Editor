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

implementation

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
  with Control as TComboBox do begin
    {TODO: не отображается прозачная заливка}
    Canvas.FillRect(ARect);
    Canvas.Brush.Style := TFPBrushStyle(Index);
    Canvas.Brush.Color := clREd;
    Canvas.FillRect(ARect);
    Canvas.Font.Color := clBlack;
    Canvas.TextOut(ARect.Left, ARect.Top, Items[Index]);
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
    for i := 0 to 7 do Items.Add(' ');
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
  with Control as TComboBox, ARect do begin
    {TODO: как-то не работает}
    Canvas.FillRect(ARect);
    Canvas.Pen.Style := TFPPenStyle(Index);
    Canvas.Pen.Width := 5;
    Canvas.Pen.Color := clBlack;
    Canvas.Line(Left, (Bottom - Top) div 2, Right, (Bottom - Top) div 2);
    Font.Color := clBlack;
    Canvas.TextOut(Left,Top, Items[index]);
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
    for i := 0 to 4 do Items.Add(' ');
    Style := csOwnerDrawFixed;
    Font.Bold := True;
    ReadOnly := True;
    Font.Size := 10;
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

