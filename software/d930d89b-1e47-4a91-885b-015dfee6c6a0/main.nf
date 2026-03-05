
process vsearch_dereplicate {
    publishDir "output/vsearch_dereplicate", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/vsearch:v1.28"

    input:
    path fq_files

    output:
    path "uniques.combine.fa"

    script:
    """
    cat ${fq_files} > all.fa

    vsearch --derep_fulllength all.fa  \
    --minuniquesize 10 \
    --sizeout --relabel Uni_ \
    --output uniques.combine.fa 
    """
}

workflow{
    ch_input = channel.fromList(params.fasta).map(it->it.fa)
    // ch_input.collect().view()
    vsearch_dereplicate(ch_input.collect())
}