#include "vm/stacktrace.hpp"

#include "vm/bytecode.hpp"
#include "vm/call_stack.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <limits>
#include <optional>

namespace insider {

static instruction_pointer const dummy_ip
  = reinterpret_cast<instruction_pointer>(
      std::numeric_limits<std::uintptr_t>::max()
    );

static std::size_t
find_index_of_call_instruction(ptr<procedure_prototype> proto,
                               instruction_pointer call_ip) {
  assert(opcode_to_info(opcode::call).num_operands == 3);
  static constexpr std::size_t call_instruction_size = 4;
  return call_ip - proto->code.get() - call_instruction_size;
}

static std::vector<std::string>
find_inlined_procedures(frame_reference frame,
                        std::optional<instruction_pointer> call_ip) {
  if (call_ip) {
    assert(*call_ip != dummy_ip);

    ptr<procedure_prototype> proto
      = assume<procedure>(frame.callable())->prototype();
    std::size_t call_idx = find_index_of_call_instruction(proto, *call_ip);
    debug_info_map const& debug_info = proto->info.debug_info;
    if (auto di = debug_info.find(call_idx); di != debug_info.end())
      return di->second.inlined_call_chain;
  }
  return {};
}

static void
append_scheme_frame_to_stacktrace(std::vector<stacktrace_record>& trace,
                                  ptr<procedure_prototype> proto,
                                  frame_reference frame,
                                  std::optional<instruction_pointer> call_ip) {
  auto inlined = find_inlined_procedures(frame, call_ip);
  for (std::string const& inlined_proc : inlined)
    trace.push_back({inlined_proc, stacktrace_record::kind::scheme});

  trace.push_back({proto->info.name, stacktrace_record::kind::scheme});
}

static void
append_callable_to_stacktrace(std::vector<stacktrace_record>& trace,
                              frame_reference frame,
                              ptr<> callable,
                              std::optional<instruction_pointer> call_ip) {
  if (auto np = match<native_procedure>(callable))
    trace.push_back({np->name, stacktrace_record::kind::native});
  else if (auto nc = match<native_continuation>(callable))
    append_callable_to_stacktrace(trace, frame, nc->proc, call_ip);
  else
    append_scheme_frame_to_stacktrace(trace,
                                      assume<procedure>(callable)->prototype(),
                                      frame,
                                      call_ip);
}

static void
append_frame_to_stacktrace(std::vector<stacktrace_record>& trace,
                           frame_reference frame,
                           std::optional<instruction_pointer> call_ip) {
  if (frame.type() != call_stack::frame_type::dummy)
    append_callable_to_stacktrace(trace, frame, frame.callable(), call_ip);
}

stacktrace
make_stacktrace(vm& state) {
  std::vector<stacktrace_record> result;
  std::optional<instruction_pointer> call_ip;
  for (call_stack::frame_index idx : state.stack.frames_range()) {
    frame_reference frame{state.stack, idx};
    append_frame_to_stacktrace(result, frame, call_ip);
    call_ip = frame.previous_ip();
  }
  return result;
}

std::string
format_stacktrace(stacktrace const& trace) {
  std::string result;
  bool first = true;
  for (auto const& [name, kind] : trace) {
    if (!first)
      result += '\n';

    result += fmt::format(
      "in {}{}",
      kind == stacktrace_record::kind::native ? "native procedure " : "",
      name
    );

    first = false;
  }

  return result;
}

} // namespace insider
