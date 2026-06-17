# bash

cd output

ln -s  "{{bed}}" .
ln -s  "{{bim}}"  .
ln -s  "{{fam}}"  .

name=$(basename -s .bed {{bed}})


/data2/brave_analysis_workspace/package/code_server/software/plink/plink \
  --bfile "${name}" \
  --mind {{mind}} \
  --make-bed \
  --out "${name}.sampleQc"

cat > {{output_dir}}/outputs.json <<EOF
{
  "bed": "{{output_dir}}/${name}.sampleQc.bed",
  "bim": "{{output_dir}}/${name}.sampleQc.bim",
  "fam": "{{output_dir}}/${name}.sampleQc.fam"
}
EOF