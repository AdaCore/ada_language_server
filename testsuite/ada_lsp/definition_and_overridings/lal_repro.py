import libadalang as lal

ctx = lal.AnalysisContext()

u = ctx.get_from_file("gb.adb")
p1_call = u.root.findall(lambda x: x.text == "P1")[0]
print p1_call.p_defining_name.p_base_subp_declarations
