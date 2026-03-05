process fastp {
    publishDir "output/fastp", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/fastp:latest"

    input:
    tuple val(meta), path(reads)

    output:
    tuple val(meta), path('*.fastp.fastq.gz') , optional:true, emit: reads
    tuple val(meta), path('*.json')           , emit: json
    tuple val(meta), path('*.html')           , emit: html

    script:
    def prefix = meta.id
    """
    fastp --in1 ${reads[0]}  --in2 ${reads[1]}  \\
        --out1 ${prefix}_1.fastp.fastq.gz  --out2 ${prefix}_2.fastp.fastq.gz \\
        --json ${prefix}.fastp.json \\
        --html ${prefix}.fastp.html \\
        --thread 10 \\
         --detect_adapter_for_pe \\
         2> ${prefix}.fastp.log
    """

    stub:
    """
    
    """
}
workflow{
    ch_input = channel.fromList(params.raw_reads).map(it->[[id:it.sample_name],[it.fastq1,it.fastq2]])
    fastp(ch_input )
}