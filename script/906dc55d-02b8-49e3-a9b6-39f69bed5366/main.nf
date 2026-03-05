process seqkit {
    container "staphb/seqkit:latest"
    publishDir "output", mode: 'copy', overwrite: true


    input:
    path(fastq_files)
    val(name)
    val(suffix)

    output:
    path("*.tsv"),emit:tsv

    script:
    """
    seqkit -j ${task.cpus} stats ${suffix} -T  -a > ${name}.tsv 

    """
}
process seqkit_plot{
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/maaslin2:1.22"
    publishDir "output", mode: 'copy', overwrite: true

    input:
    path(tsv)
    output:
    path("*.png")
    script:
    """
    Rscript ${params.pipeline_dir}/script/906dc55d-02b8-49e3-a9b6-39f69bed5366/bin/seqkit_plot.R ${tsv}  
    """

}


workflow{
    ch_input = channel.fromList(params.fasta).map(it->[[id:it.sample_name],[it.fa]])

    seqkit(ch_input.collect{it[1]},"seqkit",params.suffix) |seqkit_plot 
    // seqkit.out.tsv.view()
    // seqkit_plot()

}