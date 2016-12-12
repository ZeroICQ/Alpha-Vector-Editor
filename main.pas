unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Math, Dialogs, Menus,
  ExtCtrls, StdCtrls, aboutprogram, LCLType, Spin, Buttons, ActnList, Grids,
  UFigures, UTools, UTransform, Types;

type

  { TVectorEditor }
  TVectorEditor = class(TForm)
    ColorDialog: TColorDialog;
    ImageBoundsLabel: TLabel;
    ImageBoundsX: TLabel;
    ImageBoundsY: TLabel;
    ColorPanel: TPanel;
    ToolButtonPanel: TPanel;
    ParamPanel: TPanel;
    ScrollbarMinLabel: TLabel;
    ScrollbarMaxLabel: TLabel;
    ScrollbarPosLabel: TLabel;
    ScrollbarsLabel: TLabel;
    MouseWrldLabel: TLabel;
    MouseXWrldLabel: TLabel;
    MouseYWrldLabel: TLabel;
    MouseYDspLabel: TLabel;
    MouseXDspLabel: TLabel;
    MouseDspLabel: TLabel;
    OffsetLabel: TLabel;
    OffsetYLabel: TLabel;
    OffsetXLabel: TLabel;
    PercentLabel: TLabel;
    DebugPanel: TPanel;
    ScaleLabel: TLabel;
    ShowEverythingMenuItem: TMenuItem;
    ScaleFloatSpinEdit: TFloatSpinEdit;
    PaletteGrid: TDrawGrid;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FileDividerMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    BrushColorPanel: TPanel;
    PenColorPanel: TPanel;
    HorizontalScrollBar: TScrollBar;
    VerticalScrollBar: TScrollBar;
    ToolPanel: TPanel;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure PaletteGridDblClick(Sender: TObject);
    procedure PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScaleFloatSpinEditChange(Sender: TObject);
    procedure ShowEverythingMenuItemClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure CreateToolsButtons(ABtnWidth, ABtnHeight, AColsCount: Integer);
    procedure FillPalette;
    procedure ClearCanvas;
    procedure SetScrollBarsPostions;
    procedure SaveFigure(Figure: TFigure);
    procedure RedefineImageBounds;
    procedure UpdateScale;
    procedure VerticalScrollBarChange(Sender: TObject);
    procedure UpdateDimensions;
  private
    { private declarations }

  public
    { public declarations }

  end;

var
  VectorEditor: TVectorEditor;

implementation

{$R *.lfm}

var
  PenColor: TColor = clBlack;
  BrushColor: TColor = clBlue;
  isDrawing: Boolean = False;
  ImageBounds: TDoubleRect;
  PaletteColors: array of array of TColor;
  CurrentTool: TTool;

{ TVectorEditor }

procedure TVectorEditor.UpdateScale;
begin
  ScaleFloatSpinEdit.Value := Scale * 100;
end;

procedure TVectorEditor.SaveFigure(Figure: TFigure);
begin
  if Figure <> nil then begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := Figure;
  end;
end;

procedure TVectorEditor.RedefineImageBounds;
var i: Integer;
begin
  for i := Low(Figures) to High(Figures) do begin
      with Figures[i].GetBounds do begin
        if i = 0 then begin
          ImageBounds.Left := Left;
          ImageBounds.Top := Top;
          ImageBounds.Right := Right;
          ImageBounds.Bottom := Bottom;
        end
        else begin
          ImageBounds.Left := Min(ImageBounds.left, Left);
          ImageBounds.Top := Min(ImageBounds.Top, Top);
          ImageBounds.Right := Max(ImageBounds.Right, Right);
          ImageBounds.Bottom := Max(ImageBounds.Bottom, Bottom);
        end;
      end;
  end;
end;

procedure TVectorEditor.ClearCanvas;
var i: Integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Free;
  Figures := nil;
  //RedefineImageBounds(DoubleRect(0, 0, 0, 0)); работает несовсем правильно
  SetCanvasOffset(0, 0);
  Scale := 1.0;
  PaintBox.Invalidate;
end;

procedure TVectorEditor.UpdateDimensions;
begin
  DispDimensions := Dimensions(
    PaintBox.ClientWidth - 1,
    PaintBox.ClientHeight - 1);
end;

procedure TVectorEditor.SetScrollBarsPostions;
var
  HorPosition, HorMin, HorMax, HorPageSize: Integer;
  VertPosition, VertMin, VertMax, VertPageSize: Integer;
begin
  HorPosition := round(GetCanvasOffset.X);
  HorPageSize := DispDimensions.Width;
  HorMin := WorldToDispDimension(ImageBounds.Left);
  if HorPosition < HorMin then
    HorMin := HorPosition;
  HorMax :=  WorldToDispDimension(ImageBounds.Right);
  if HorPosition > HorMax - HorPageSize then
    HorMax := HorPosition + HorPageSize;
  HorizontalScrollBar.SetParams(HorPosition, HorMin, HorMax, HorPageSize);

  VertPosition := round(GetCanvasOffset.Y);
  VertPageSize := DispDimensions.Height;
  VertMin := WorldToDispDimension(ImageBounds.Top);
  if VertPosition < VertMin then
    VertMin := VertPosition;
  VertMax :=  WorldToDispDimension(ImageBounds.Bottom);
  if VertPosition > VertMax - VertPageSize then
    VertMax := VertPosition + VertPageSize;
  VerticalScrollBar.SetParams(VertPosition, VertMin, VertMax, VertPageSize);
 end;

procedure TVectorEditor.HorizontalScrollBarChange(Sender: TObject);
begin
  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
      Exit;
    end;
  end;

  with Sender as TScrollBar do
    SetCanvasOffset(Position, GetCanvasOffset.Y);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.VerticalScrollBarChange(Sender: TObject);
begin
  with Sender as TScrollBar do begin
    if Position > Max - PageSize then begin
      Position := Max - PageSize;
      Exit;
    end;
  end;
  with Sender as TScrollBar do
    SetCanvasOffset(GetCanvasOffset.X, Position);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ToolClick(Sender: TObject);
begin
  CurrentTool := Tools[(Sender as TSpeedButton).Tag];
  CurrentTool.Init(ParamPanel)
end;

procedure TVectorEditor.CreateToolsButtons(
  ABtnWidth, ABtnHeight, AColsCount: Integer);
var
  i: Integer;
  ToolBtn: TSpeedButton;
  ToolIcon: TBitmap;
begin
  for i := 0 to High(Tools) do begin
    ToolBtn := TSpeedButton.Create(VectorEditor);
    ToolIcon := TBitmap.Create;
    with ToolIcon do begin
      TransparentColor := clWhite;
      Transparent := True;
      LoadFromFile(Tools[i].Icon);
    end;
    with ToolBtn do begin
      Glyph := ToolIcon;
      Flat := True;
      Width := ABtnWidth;
      Height := ABtnHeight;
      Top := (i div AColsCount) * ABtnHeight;
      Left := (i mod AColsCount) * ABtnWidth;
      Tag := i;
      GroupIndex := 1;
      OnClick := @ToolClick;
      if i = 0 then Down := True;
      Parent := ToolButtonPanel;
    end;
  end;
end;

procedure TVectorEditor.FillPalette;
var
  col, row, rate, index: Integer;
begin
  index := 0;
  rate := floor(255 / (PaletteGrid.RowCount * PaletteGrid.ColCount));
  for row := 0 to PaletteGrid.RowCount do begin
    SetLength(PaletteColors, Length(PaletteColors) + 1);
    for col := 0  to PaletteGrid.ColCount do begin
      SetLength(PaletteColors[row], Length(PaletteColors[row]) + 1);
      PaletteColors[row, col] := RGBToColor(index * rate, col * 28,
        (PaletteGrid.ColCount - row) * 42);
      index += 1;
    end;
  end;
end;

procedure TVectorEditor.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditor.FormCreate(Sender: TObject);
var
  BtnWidth, BtnHeight, ColsCount: Integer;
begin
  CurrentTool := Tools[0];
  CurrentTool.Init(ParamPanel);
  //Передаём дефолтный параметры представлению
  PenColorPanel.Color := PenColor;
  BrushColorPanel.Color := BrushColor;
  ScaleFloatSpinEdit.Value := 100;
  //Параметры кнопок  интрументов
  BtnWidth := 48;
  BtnHeight := 48;
  ColsCount := 3;
  CreateToolsButtons(BtnWidth, BtnHeight, ColsCount);
  //Палитра
  FillPalette;
  UpdateDimensions;
end;

procedure TVectorEditor.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := True;
  CurrentTool.MouseDown(Point(X, Y), PenColor, BrushColor, Button, Shift);
end;

procedure TVectorEditor.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if isDrawing then begin
    CurrentTool.MouseMove(Point(X, Y));
    PaintBox.Invalidate;
  end;
  {DEBUG}
  MouseXDspLabel.Caption := 'x: ' + FloatToStr(X);
  MouseYDspLabel.Caption := 'y: ' + FloatToStr(Y);
  MouseXWrldLabel.Caption := 'x: ' + FloatToStr(DispToWorldCoord(X,Y).X);
  MouseYWrldLabel.Caption := 'y: ' + FloatToStr(DispToWorldCoord(X,Y).Y);
end;

procedure TVectorEditor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := False;
  CurrentTool.MouseUp(Point(X, Y));
  SaveFigure(CurrentTool.GetFigure);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos);
  DecreaseScale;
  AddCanvasOffset((StartingMouseCrds - DispToWorldCoord(MousePos)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  StartingMouseCrds: TDoublePoint;
begin
  StartingMouseCrds := DispToWorldCoord(MousePos);
  IncreaseScale;
  AddCanvasOffset((StartingMouseCrds - DispToWorldCoord(MousePos)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.PaintBoxPaint(Sender: TObject);
var i:integer;
begin
  RedefineImageBounds;
  for i := 0 to High(Figures) do begin
    Figures[i].Draw(PaintBox);
  end;
  if isDrawing and (CurrentTool.GetFigure <> nil) then begin
    CurrentTool.GetFigure.Draw(PaintBox);
  end;
  UpdateScale;
  SetScrollBarsPostions;
  {DEBUG}
  OffsetXLabel.Caption := 'x: ' + FloatToStr(GetCanvasOffset.X);
  OffsetYLabel.Caption := 'y: ' + FloatToStr(GetCanvasOffset.Y);
  ImageBoundsX.Caption := 'left: ' + FloatToStr(ImageBounds.Left);
  ImageBoundsY.Caption := 'top: ' + FloatToStr(ImageBounds.Top);
  ScrollbarMinLabel.Caption := 'Min: ' + IntToStr(HorizontalScrollBar.Min);
  ScrollbarMaxLabel.Caption := 'Max: ' + IntToStr(HorizontalScrollBar.Max);
  ScrollbarPosLabel.Caption := 'Pos: ' + IntToStr(HorizontalScrollBar.Position);
end;

procedure TVectorEditor.PaintBoxResize(Sender: TObject);
begin
  UpdateDimensions;
end;

procedure TVectorEditor.PaletteGridDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
    with Sender as TDrawGrid do begin
      PaletteColors[Row, COl] := ColorDialog.Color;
    end;
    PaletteGrid.Invalidate;
  end;
end;

procedure TVectorEditor.PaletteGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  PaletteGrid.Canvas.Brush.Color := PaletteColors[aRow, aCol];
  PaletteGrid.Canvas.FillRect(aRect);
end;

procedure TVectorEditor.PaletteGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  PaletteGrid.MouseToCell(X, Y, ACol, ARow);
  if Button = mbLeft then  begin
    PenColor := PaletteColors[ARow, ACol];
    PenColorPanel.Color := PenColor;
  end;
  if Button = mbRight then begin
    BrushColor := PaletteColors[ARow, ACol];
    BrushColorPanel.Color := BrushColor;
  end;
end;

procedure TVectorEditor.ScaleFloatSpinEditChange(Sender: TObject);
var
  CenterDspCrds: TPoint;
  StartCenterWrldCrds: TDoublePoint;
begin
  CenterDspCrds := Point(
    round(DispDimensions.Width  / 2), round(DispDimensions.Height / 2));
  StartCenterWrldCrds := DispToWorldCoord(CenterDspCrds);
  Scale := (Sender as TFloatSpinEdit).Value / 100;
  AddCanvasOffset((StartCenterWrldCrds - DispToWorldCoord(CenterDspCrds)) * Scale);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.ShowEverythingMenuItemClick(Sender: TObject);
const
  BorderMargin = 5;//px
var
  XScale, YScale: Double;
  ImgWorldWidth, ImgWorldHeight: Double;
begin
  ImgWorldWidth := ImageBounds.Right - ImageBounds.Left;
  ImgWorldHeight := ImageBounds.Bottom - ImageBounds.Top;
  XScale := DispDimensions.Width / (ImgWorldWidth + 2 * BorderMargin / Scale);
  YScale := DispDimensions.Height / (ImgWorldHeight + 2 * BorderMargin / Scale);
  Scale := Min(XScale, YScale);
  //Размещение по центру
  SetCanvasOffset(
    WorldToDispDimension(ImageBounds.Left) -
      (DispDimensions.Width - WorldToDispDimension(ImgWorldWidth)) / 2,
    WorldToDispDimension(ImageBounds.Top) -
      (DispDimensions.Height - WorldToDispDimension(ImgWorldHeight)) / 2);
  PaintBox.Invalidate;
end;

procedure TVectorEditor.AboutMenuItemClick(Sender: TObject);
begin
  aboutprogram.aboutProgramForm.Show;
end;

procedure TVectorEditor.ClearMenuItemClick(Sender: TObject);
begin
  ClearCanvas;
end;

end.
