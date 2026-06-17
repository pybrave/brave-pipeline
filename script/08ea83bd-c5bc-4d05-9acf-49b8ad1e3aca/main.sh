# bash

cd output

ln -s  "{{bed}}" .
ln -s  "{{bim}}"  .
ln -s  "{{fam}}"  .

name=$(basename -s .bed {{bed}})


/data2/brave_analysis_workspace/package/code_server/software/plink/plink \
  --bfile ${name} \
  --geno {{geno}} \
  --maf {{maf}} \
  --hwe {{hwe}} \
  --make-bed \
  --out ${name}.snpQC


cat > {{output_dir}}/outputs.json <<EOF
{
  "bed": "{{output_dir}}/${name}.snpQC.bed",
  "bim": "{{output_dir}}/${name}.snpQC.bim",
  "fam": "{{output_dir}}/${name}.snpQC.fam"
}
EOF