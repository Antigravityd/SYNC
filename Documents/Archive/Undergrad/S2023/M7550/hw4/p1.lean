import linear_algebra

variables {V W F : Type} [vector_space V] [vector_space W] [field F]

theorem (f : V →₁[F] W) (hfs : function.surjective ⇑f) : function.injective ⇑(f.dual_map) :=
  begin
    sorry
  end
