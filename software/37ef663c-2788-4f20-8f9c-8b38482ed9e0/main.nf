
process vsearch_dereplicate {
    publishDir "output/usearch", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/usearch"

    input:
    path fq_files

    output:
    path "zotus.fa"

    script:
    """
    cat ${fq_files} > all.fa
    usearch -unoise3 all.fa  -minsize 10 -zotus zotus.fa
    rm all.fa
    """
}

workflow{
    ch_input = channel.fromList(params.fasta).map(it->it.fa)
    // ch_input.collect().view()
    vsearch_dereplicate(ch_input.collect())
}