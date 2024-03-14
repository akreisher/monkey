package compiler

type SymbolScope string

const (
	GlobalScope  SymbolScope = "GLOBAL"
	LocalScope   SymbolScope = "LOCAL"
	BuiltinScope SymbolScope = "BUILTIN"
	FreeScope    SymbolScope = "FREE"
)

type Symbol struct {
	Name  string
	Scope SymbolScope
	Index int
}

type SymbolTable struct {
	store          map[string]Symbol
	numDefinitions int
	Outer          *SymbolTable
	FreeSymbols    []Symbol
}

func NewSymbolTable() *SymbolTable {
	s := make(map[string]Symbol)
	return &SymbolTable{store: s}
}

func NewEnclosedSymbolTable(Outer *SymbolTable) *SymbolTable {
	newTable := NewSymbolTable()
	newTable.Outer = Outer
	return newTable
}

func (st *SymbolTable) Define(name string) Symbol {
	symbol := Symbol{Name: name, Index: st.numDefinitions}

	if st.Outer == nil {
		symbol.Scope = GlobalScope
	} else {
		symbol.Scope = LocalScope
	}

	st.store[name] = symbol
	st.numDefinitions++
	return symbol
}

func (st *SymbolTable) defineFree(original Symbol) Symbol {
	st.FreeSymbols = append(st.FreeSymbols, original)

	symbol := Symbol{Name: original.Name, Index: len(st.FreeSymbols) - 1, Scope: FreeScope}
	st.store[original.Name] = symbol

	return symbol
}

func (st *SymbolTable) Resolve(name string) (Symbol, bool) {
	val, ok := st.store[name]
	if !ok && st.Outer != nil {
		val, ok = st.Outer.Resolve(name)
		if !ok {
			return val, ok
		}
		if val.Scope == GlobalScope || val.Scope == BuiltinScope {
			return val, ok
		}
		return st.defineFree(val), ok
	}
	return val, ok
}

func (st *SymbolTable) DefineBuiltin(index int, name string) Symbol {
	symbol := Symbol{Name: name, Index: index, Scope: BuiltinScope}
	st.store[name] = symbol
	return symbol
}
