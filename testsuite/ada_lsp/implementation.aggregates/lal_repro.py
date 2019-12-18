import libadalang as lal

provider_p = lal.UnitProvider.for_project("agg.gpr", project="p/p.gpr")
ctx_p = lal.AnalysisContext(unit_provider=provider_p)

provider_q = lal.UnitProvider.for_project("agg.gpr", project="q/q.gpr")
ctx_q = lal.AnalysisContext(unit_provider=provider_q)

u_p = ctx_p.get_from_file("common/common_pack.ads")
u_q = ctx_q.get_from_file("common/common_pack.ads")

print u_p
print u_q

fun_p = u_p.root.findall(lambda x: x.text == "Common_Fun")[0]
print fun_p.p_next_part.unit.filename

fun_q = u_q.root.findall(lambda x: x.text == "Common_Fun")[0]
print fun_q.p_next_part.unit.filename

fun_p = u_p.root.findall(lambda x: x.text == "Common_Fun")[0]
print fun_p.p_next_part.unit.filename

fun_q = u_q.root.findall(lambda x: x.text == "Common_Fun")[0]
print fun_q.p_next_part.unit.filename


#print p1_call.p_defining_name.p_base_subp_declarations
