
i = KI{Ingredients.Salt.amount=1}
br = KBr{Ingredients.Salt.amount=4}
cl = NaCl{Ingredients.Salt.amount=3}

mergeSalts [i,br,cl] [i,br,cl]

sn = (SILVERNITRATE {Ingredients.SilverNitrate.amount=10})
reactAGX [] sn [i,br,cl]
reactAGX [] sn [i]
reactAGX [] sn [br]
reactAGX [] sn [cl]


sn = (SILVERNITRATE {Ingredients.SilverNitrate.amount=5})
br = KBr{Ingredients.Salt.amount=1}
cl = NaCl{Ingredients.Salt.amount=2}
reactAGX [] sn [br,cl]

