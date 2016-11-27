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
    procedure OnLineStyleChange(Sender: TObject);
    constructor Create(ALineStyleChange: TLineStyleChange);
  end;

  { TBrushStyleParameter }

  TBrushStyleParameter = class(TParameter)
    FBrushStyleChange: TBrushStyleChange;
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

implementation

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

procedure TBrushStyleParameter.OnBrushStyleChange(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FBrushStyleChange(TFPBrushStyle(ItemIndex));
  end;
end;

constructor TBrushStyleParameter.Create(ABrushStyleChange: TBrushStyleChange);
begin
  Inherited Create;
  FBrushStyleChange := ABrushStyleChange;
  FLabel.Caption := 'Стиль заливки';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    Items.Add('Сплошная');
    Items.Add('Без заливки');
    Items.Add('Горизонтальная');
    Items.Add('Вертикальная');
    Items.Add('Диагональная 1');
    Items.Add('Диагональная 2');
    Items.Add('Крестом');
    Items.Add('Наклонным крестом');
    Font.Size := 10;
    Width := 130;
    ItemIndex := 0;
    OnChange := @OnBrushStyleChange;
  end;
end;

{ TLineStyleParameter }

procedure TLineStyleParameter.OnLineStyleChange(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyleChange(TFPPenStyle(ItemIndex));
  end;
end;

constructor TLineStyleParameter.Create(ALineStyleChange: TLineStyleChange);
begin
  Inherited Create;
  FLineStyleChange := ALineStyleChange;
  FLabel.Caption := 'Стиль линии';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    Items.Add('─────');
    Items.Add('─ ─ ─ ─ ─');
    Items.Add('• • • • • • • • •');
    Items.Add('─ • ─ • ─ •');
    Items.Add('─ • • ─ • •');
    Style := csOwnerDrawFixed;
    Font.Bold := True;
    Font.Size := 10;
    Width := 130;
    ItemIndex := 0;
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

