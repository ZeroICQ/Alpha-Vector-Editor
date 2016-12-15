unit UParamEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, UTransform, StdCtrls,
  ExtCtrls, Spin, FPCanvas, UParameters;

type

  { TParamEditor }

  TParamEditor = class
  private
    FLabel: TLabel;
    FComponent: TControl;
  public
    procedure SetParam(AParam: TParam); virtual; abstract;
    function GetParamType: TParamClass; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLineWidthParamEditor }

  TLineWidthParamEditor = class(TParamEditor)
  public
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    constructor Create;
  end;

  { TLineStyleParamEditor }

  TLineStyleParamEditor = class(TParamEditor)
  private
    procedure OnDrawLineStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    constructor Create;
  end;

  { TBrushStyleParamEditor }

  TBrushStyleParamEditor = class(TParamEditor)
  private
    procedure OnDrawBrushStyleItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    constructor Create;
  end;

  { TCornersParamEdtitor }

  TCornersParamEdtitor = class(TParamEditor)
  public
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    constructor Create;
  end;

  { TIntegerParamEditor }

  TIntegerParamEditor = class(TParamEditor)
  public
    procedure SetParam(AParam: TParam); override;
    function GetParamType: TParamClass; override;
    constructor Create(ACaption: String; AValue: Integer = 1);
  end;

  procedure ShowParamEditors(AParams: array of TParamEditor; APanel: TPanel);

implementation

uses
  LCLType;

{ Misc }

procedure ShowParamEditors(AParams: array of TParamEditor; APanel: TPanel);
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

{ TIntegerParamEditor }

procedure TIntegerParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue((FComponent as TSpinEdit).Value);
end;

function TIntegerParamEditor.GetParamType: TParamClass;
begin
  Result := TParamInteger;
end;

constructor TIntegerParamEditor.Create(ACaption: String; AValue: Integer = 1);
begin
  Inherited Create;
  FLabel.Caption := ACaption;
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 1000;
    MinValue := 1;
    Value := AValue;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 130;
    //OnChange := @OnFactorChange;
  end;
end;

{ TCornersParamEdtitor }

procedure TCornersParamEdtitor.SetParam(AParam: TParam);
begin
  AParam.SetValue((FComponent as TSpinEdit).Value);
end;

function TCornersParamEdtitor.GetParamType: TParamClass;
begin
  Result := TParamCorners;
end;

constructor TCornersParamEdtitor.Create;
begin
  Inherited Create;
  FLabel.Caption := 'Количество углов';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 15;
    MinValue := 3;
    Value := 3;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    //OnChange := @OnCornersNumberChange;
  end;
end;

{ TBrushStyleParamEditor }

procedure TBrushStyleParamEditor.OnDrawBrushStyleItem(
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

procedure TBrushStyleParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue(TFPBrushStyle((FComponent as TComboBox).ItemIndex));
end;

function TBrushStyleParamEditor.GetParamType: TParamClass;
begin
  Result := TParamBrushStyle;
end;

constructor TBrushStyleParamEditor.Create;
var i: Integer;
begin
  Inherited Create;
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
    //OnChange := @OnBrushStyleChange;
  end;
end;

{ TLineStyleParamEditor }

procedure TLineStyleParamEditor.OnDrawLineStyleItem(Control: TWinControl;
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

procedure TLineStyleParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue(TFPPenStyle((FComponent as TComboBox).ItemIndex));
end;

function TLineStyleParamEditor.GetParamType: TParamClass;
begin
  Result := TParamLineStyle;
end;

constructor TLineStyleParamEditor.Create;
var i: Integer;
begin
  Inherited Create;
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
    //OnChange := @OnLineStyleChange;
  end;
end;

{ TParamEditor }

constructor TParamEditor.Create;
begin
  Inherited;
  FLabel := TLabel.Create(nil);
  FLabel.Font.Size := 11;
end;

destructor TParamEditor.Destroy;
begin
  FLabel.Free;
  FComponent.Free;
  inherited Destroy;
end;

{ TLineWidthParamEditor }

procedure TLineWidthParamEditor.SetParam(AParam: TParam);
begin
  AParam.SetValue((FComponent as TSpinEdit).Value);
end;

function TLineWidthParamEditor.GetParamType: TParamClass;
begin
  Result := TParamLineWidth;
end;

constructor TLineWidthParamEditor.Create;
begin
  Inherited Create;
  FLabel.Caption := 'Толщина линии';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 500;
    MinValue := 1;
    Value := 3;
    Font.Size := 11;
    Alignment := taRightJustify;
    Width := 64;
    //OnChange := @OnLineWidthChange;
  end;
end;

end.

