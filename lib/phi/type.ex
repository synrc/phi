defmodule Phi.Type do
  @moduledoc """
  Core type definitions for the typechecker.
  Distinct from the parser AST because these represent inferred
  and unified internal types (monotypes and polytypes).
  """

  defmodule TVar do
    defstruct [:id]
  end

  defmodule TCon do
    defstruct [:name]
  end

  defmodule TApp do
    defstruct [:func, :arg]
  end

  defmodule Forall do
    defstruct [:vars, :type]
  end

  defmodule TRowEmpty do
    @moduledoc "The empty row, capping a row extension"
    defstruct []
  end

  defmodule TRowExtend do
    @moduledoc "Extends a row with a new label and type"
    defstruct [:label, :type, :rest]
  end

  defmodule TConstrained do
    @moduledoc "A type constrained by a type class (e.g. `Eq a => a -> a -> Bool`)"
    defstruct [:class_name, :args, :type]
  end

  # Helper to construct Arrow types
  def arrow(t1, t2) do
    %TApp{func: %TApp{func: %TCon{name: "->"}, arg: t1}, arg: t2}
  end

  # Primitive Types
  def ty_int(), do: %TCon{name: "Int"}
  def ty_bool(), do: %TCon{name: "Bool"}
  def ty_string(), do: %TCon{name: "String"}
end
