GEN_JSON    = gen_json
OUT_DIR     = ./generated
GNATPP_OPTS = -P ../../gnat/lsp_gen.gpr
DOC         = ../../doc

all: create pretty build generate

create: ${DOC}/metaModel.schema.json header.adt
	${GEN_JSON} \
	    --root-package LSP_Gen.Entities --enum-package Enum \
	    --header-file header.adt \
	    --holder "#/definitions/ArrayType:element" \
	    --holder "#/definitions/MapType:value" \
	    ${DOC}/metaModel.schema.json > ada.txt
	gnatchop -w ada.txt ${OUT_DIR}

pretty:
	gnatpp ${GNATPP_OPTS} ${OUT_DIR}/*.ad[sb]

build: pretty
	gprbuild ${GNATPP_OPTS}

generate: build
	../../.obj/gs/lsp_gen/lsp_gen-run ${DOC}/metaModel.json config/3_17.json5 > /tmp/aaa.txt
