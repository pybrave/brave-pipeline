nextflow.enable.dsl=2

process generate_plot {
    publishDir  "output"
    input:
    path script

    output:
    path "output.png"

    """
    sleep 33
    draw_plot.py output.png 
    """
}

workflow {
    // 提供 Python 脚本路径
    script_ch = Channel.fromPath("draw_plot.py")

    generate_plot(script_ch)
}
