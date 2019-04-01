import math

grams_gelatin = 550
grams_agno = 96
grams_ki = 0.2
grams_kbr = 20
grams_nacl = 23.5
total_water = 1625
mils_hcl = 0

ki_mw = 166.00
kbr_mw = 119.0075
nacl_mw = 58.4428
agno_mw = 169.8749
agcl_mw = 143.32
agbr_mw = 187.77
agi_mw = 234.77

hcl_molarity = 10.2

# react_amt_kbr = 0.7
# react_amt_nacl = 0.408

# ----

moles_agno = grams_agno / agno_mw
reacting_agno = moles_agno

# Mol conversion
moles_ki = grams_ki / ki_mw
moles_kbr = grams_kbr / kbr_mw
moles_nacl = grams_nacl / nacl_mw

# ====

if reacting_agno >= moles_ki:
	moles_agi = moles_ki
	reacting_agno = reacting_agno - moles_agi
else:
	moles_agi = reacting_agno
	reacting_agno = 0.0
grams_agi = moles_agi * agi_mw

# ====

if reacting_agno >= moles_kbr:
	moles_agbr = moles_kbr
	reacting_agno = reacting_agno - moles_agbr
else:
	moles_agbr = reacting_agno
	reacting_agno = 0.0
grams_agbr = moles_agbr * agbr_mw

# ======

if reacting_agno >= moles_nacl:
	moles_agcl = moles_nacl
	reacting_agno = reacting_agno - moles_agcl
else:
	moles_agcl = reacting_agno
	reacting_agno = 0.0
grams_agcl = moles_agcl * agcl_mw


print("""
Your reactants are:\n
Gelatin: {}\n
AgNO3: {}\n
KI: {}\n
KBr: {}\n
NaCl: {}\n
Water: {}\n
HCl: {}\n
""".format(
    grams_gelatin,
    grams_agno,
    grams_ki,
    grams_kbr,
    grams_nacl,
    total_water,
    mils_hcl
    ))

print("Ratio gelatin to water {}".format(grams_gelatin / float(total_water)))
rec = grams_gelatin / 0.109
print("(Recommend {} mils water".format(rec))

if reacting_agno == 0:
    print("Ran out of silver. Would need {} more grams.".format(((moles_kbr + moles_nacl)-moles_agno)*agno_mw))


if mils_hcl > 0.0:
    print("Best guess acidity: {}".format(-1*math.log( (hcl_molarity* (float(mils_hcl)/1000))/total_water )))
else:
    print("Unknown ph")

print("{}% AgI. {}% AgBr. {}% AgCl (by mol)".format(
        (moles_agi/(moles_agbr + moles_agcl + moles_agi)),
        (moles_agbr/(moles_agbr + moles_agcl + moles_agi)),
        (moles_agcl/(moles_agbr + moles_agcl + moles_agi))
    )
)

print("Leftover Silver: {}".format(reacting_agno * agno_mw))

print("Ratio of salts to silver is, {}".format((moles_nacl + moles_kbr + moles_agi) / moles_agno))

print("Your ratio gel/AgX is {}".format(grams_gelatin / (grams_agi + grams_agcl + grams_agbr)))