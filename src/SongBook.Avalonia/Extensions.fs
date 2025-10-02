namespace SongBook.Avalonia

[<AutoOpen>]
module SymbolIcon =
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder
    open FluentIcons.Avalonia
    open Avalonia.FuncUI.DSL
    open FluentIcons.Common

    let create (attrs: IAttr<SymbolIcon> list) : IView<SymbolIcon> = ViewBuilder.Create<SymbolIcon> attrs

    type SymbolIcon with
        static member symbol<'t when 't :> SymbolIcon>(value: Symbol) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<Symbol>(SymbolIcon.SymbolProperty, value, ValueNone)

        static member iconVariant<'t when 't :> SymbolIcon>(value: IconVariant) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<IconVariant>(SymbolIcon.IconVariantProperty, value, ValueNone)

[<AutoOpen>]
module DataGrid =
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder
    open Avalonia.Controls

    type DataGrid with
        static member headersVisibility<'t when 't :> DataGrid>(value: DataGridHeadersVisibility) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<DataGridHeadersVisibility>(DataGrid.HeadersVisibilityProperty, value, ValueNone)
