import xml.dom.minidom
from condition import condition_to_cpp

api_xmls = ["../libsel4/arch_include/arm/interfaces/sel4arch.xml",
"../libsel4/arch_include/riscv/interfaces/sel4arch.xml",
"../libsel4/arch_include/x86/interfaces/sel4arch.xml",
"../libsel4/sel4_arch_include/aarch32/interfaces/sel4arch.xml",
"../libsel4/sel4_arch_include/aarch64/interfaces/sel4arch.xml",
"../libsel4/sel4_arch_include/ia32/interfaces/sel4arch.xml",
"../libsel4/sel4_arch_include/riscv32/interfaces/sel4arch.xml",
"../libsel4/sel4_arch_include/riscv64/interfaces/sel4arch.xml",
"../libsel4/sel4_arch_include/x86_64/interfaces/sel4arch.xml",
"../libsel4/include/interfaces/sel4.xml"
]

syscalls = ["../libsel4/include/api/syscall.xml"]

def main():
    for xml_file in api_xmls:
        print(xml_file)
        doc = xml.dom.minidom.parse(xml_file)
        configs = doc.getElementsByTagName("method")
        for config in configs:
            condition_attr = config.getAttribute("condition")
            condition_elem = config.getElementsByTagName("condition")
            cond_cpp = condition_to_cpp(condition_elem)
            if not condition_attr == cond_cpp:
                print("\tbroken attr: {} elem: {}".format(condition_attr, cond_cpp))
            elif len(condition_attr) != 0:
                print("\tOK {}".format(cond_cpp))
            assert condition_attr == cond_cpp

    for xml_file in syscalls:
        print(xml_file)
        doc = xml.dom.minidom.parse(xml_file)
        configs = doc.getElementsByTagName("config")
        for config in configs:
            condition_attr = config.getAttribute("condition")
            condition_elem = config.getElementsByTagName("condition")
            cond_cpp = condition_to_cpp(condition_elem)
            if not condition_attr == cond_cpp:
                print("\tbroken attr: {} elem: {}".format(condition_attr, cond_cpp))
            assert condition_attr == cond_cpp

if __name__ == "__main__":
    main()

