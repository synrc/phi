defmodule Phi.AST do
  @moduledoc """
  Abstract Syntax Tree structures for the Phi compiler.
  """

  defmodule Module do
    defstruct [:name, :declarations]
  end

  defmodule DeclValue do
    defstruct [:name, :binders, :expr, :guards]
  end

  defmodule DeclTypeSignature do
    defstruct [:name, :type]
  end

  defmodule DeclData do
    defstruct [:name, :args, :constructors]
  end

  defmodule DeclTypeAlias do
    defstruct [:name, :args, :type]
  end

  defmodule DeclNewtype do
    defstruct [:name, :args, :constructor, :type]
  end

  defmodule BinderTuple do
    defstruct [:elems]
  end

  defmodule BinderList do
    defstruct [:head, :tail]
  end

  defmodule DeclImport do
    defstruct [:module, :import_list, :alias, :hiding]
  end

  defmodule DeclFixity do
    defstruct [:prec, :op, :name, :assoc]
  end

  defmodule DeclForeign do
    defstruct [:name, :type]
  end

  defmodule ExprVar do
    defstruct [:name]
  end

  defmodule ExprApp do
    defstruct [:func, :arg]
  end

  defmodule ExprLam do
    defstruct [:binder, :body]
  end

  defmodule ExprLet do
    defstruct [:bindings, :body]
  end

  defmodule ExprTuple do
    defstruct [:elems]
  end

  defmodule ExprList do
    defstruct [:elems, :tail]
  end

  defmodule ExprIf do
    defstruct [:cond, :then_br, :else_br]
  end

  defmodule TypeVar do
    defstruct [:name]
  end

  defmodule TypeConstructor do
    defstruct [:name]
  end

  defmodule TypeApp do
    defstruct [:func, :arg]
  end

  defmodule TypeTuple do
    defstruct [:elems]
  end

  defmodule TypeList do
    defstruct [:element]
  end

  defmodule TypeConstrained do
    defstruct [:constraints, :type]
  end

  defmodule TypeArrow do
    defstruct [:domain, :codomain]
  end

  defmodule TypeForall do
    defstruct [:vars, :type]
  end

  defmodule DeclClass do
    defstruct [:name, :args, :members]
  end

  defmodule DeclInstance do
    defstruct [:name, :class, :types, :members]
  end

  defmodule ExprCase do
    defstruct [:exprs, :branches]
  end

  defmodule BinderVar do
    defstruct [:name]
  end

  defmodule BinderConstructor do
    defstruct [:name, :args]
  end
end
