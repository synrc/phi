defmodule PhiTypecheckerTest do
  use ExUnit.Case
  alias Phi.Type.{TVar, TCon, TApp}
  alias Phi.Typechecker
  alias Phi.Typechecker.State

  test "Unification of Identical Types" do
    state = %State{}
    t1 = %TCon{name: "Int"}

    assert {:ok, _} = Typechecker.unify(t1, t1, state)
  end

  test "Unification of Type Variable and Concrete Type" do
    state = %State{}
    t_var = %TVar{id: 1}
    t_int = %TCon{name: "Int"}

    {:ok, state2} = Typechecker.unify(t_var, t_int, state)
    assert Map.get(state2.subst, 1) == t_int
  end

  test "Unification of Arrows (Functions)" do
    state = %State{}
    # a -> Int
    t1 = Phi.Type.arrow(%TVar{id: 1}, %TCon{name: "Int"})
    # String -> b
    t2 = Phi.Type.arrow(%TCon{name: "String"}, %TVar{id: 2})

    {:ok, state2} = Typechecker.unify(t1, t2, state)

    # After unification: 'a' is String, 'b' is Int
    assert Map.get(state2.subst, 1) == %TCon{name: "String"}
    assert Map.get(state2.subst, 2) == %TCon{name: "Int"}
  end

  test "Unification catches Infinite Types (Occurs Check)" do
    state = %State{}
    # Unifying `a` with `a -> Int` should fail because it results in an infinite type
    t_var = %TVar{id: 1}
    t_arrow = Phi.Type.arrow(%TVar{id: 1}, %TCon{name: "Int"})

    assert {:error, "Occurs check failed: infinite type"} = Typechecker.unify(t_var, t_arrow, state)
  end

  test "Inference for Identity Function `\\x -> x`" do
    # AST: \x -> x
    expr = %Phi.AST.ExprLam{
      binder: %Phi.AST.BinderVar{name: "x"},
      body: %Phi.AST.ExprVar{name: "x"}
    }

    env = Phi.Typechecker.Env.new()
    {:ok, type} = Phi.Typechecker.infer(env, expr)

    # Should give `t1 -> t1`
    assert %TApp{
             func: %TApp{func: %TCon{name: "->"}, arg: %TVar{id: id1}},
             arg: %TVar{id: id2}
           } = type

    assert id1 == id2
  end

  test "Inference applies polymorphism in let bindings" do
    # AST: let id = \x -> x in id
    id_func = %Phi.AST.ExprLam{
      binder: %Phi.AST.BinderVar{name: "x"},
      body: %Phi.AST.ExprVar{name: "x"}
    }

    expr = %Phi.AST.ExprLet{
      bindings: [%Phi.AST.DeclValue{name: "id", binders: [], expr: id_func}],
      body: %Phi.AST.ExprVar{name: "id"}
    }

    env = Phi.Typechecker.Env.new()
    {:ok, type} = Phi.Typechecker.infer(env, expr)

    # Polmorphic type should be instantiated, so it's `t2 -> t2`
    assert %TApp{
             func: %TApp{func: %TCon{name: "->"}, arg: %TVar{id: id1}},
             arg: %TVar{id: id2}
           } = type

    assert id1 == id2
  end

  test "Unification of Row Types (Record Polymorphism)" do
    state = %State{}
    # { x :: Int | r1 }
    r1 = %Phi.Type.TRowExtend{label: "x", type: %TCon{name: "Int"}, rest: %Phi.Type.TVar{id: 1}}

    # { y :: String, x :: a | r2 }
    r2 = %Phi.Type.TRowExtend{label: "y", type: %TCon{name: "String"},
           rest: %Phi.Type.TRowExtend{label: "x", type: %Phi.Type.TVar{id: 2}, rest: %Phi.Type.TVar{id: 3}}}

    {:ok, state2} = Typechecker.unify(r1, r2, state)

    # TVar 2 (a) should unify with Int
    assert Map.get(state2.subst, 2) == %TCon{name: "Int"}

    # TVar 1 (r1) should unify with { y :: String | r_rest }
    t1_subst = Map.get(state2.subst, 1)
    assert %Phi.Type.TRowExtend{label: "y", type: %TCon{name: "String"}} = t1_subst
  end

  test "Unification of Constrained Types (Type Classes)" do
    state = %State{}
    # Eq a => a -> a
    c1 = %Phi.Type.TConstrained{
      class_name: "Eq",
      args: [%Phi.Type.TVar{id: 1}],
      type: Phi.Type.arrow(%Phi.Type.TVar{id: 1}, %Phi.Type.TVar{id: 1})
    }

    # Eq Int => Int -> Int
    c2 = %Phi.Type.TConstrained{
      class_name: "Eq",
      args: [%TCon{name: "Int"}],
      type: Phi.Type.arrow(%TCon{name: "Int"}, %TCon{name: "Int"})
    }

    {:ok, state2} = Typechecker.unify(c1, c2, state)

    assert Map.get(state2.subst, 1) == %TCon{name: "Int"}
  end
end
